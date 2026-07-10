(in-package :cl-cc/optimize)
;;; -----------------------------------------------------------------------------
;;; Optimizer Basic Memory Alias Queries
;;; -----------------------------------------------------------------------------

(defun opt-compute-heap-kinds (instructions)
  "Compute a conservative root -> heap-kind table for TBAA-style checks."
  (let ((points-to (opt-compute-heap-aliases instructions))
        (kinds (make-hash-table :test #'eq)))
    (dolist (inst instructions kinds)
      (let ((dst (opt-inst-dst inst)))
        (when (and dst (opt-heap-root-inst-p inst))
          (let ((root (gethash dst points-to)))
            (when root
              (setf (gethash root kinds) (opt-heap-root-kind inst)))))))
    kinds))

(defun opt-may-alias-by-type-p (reg-a reg-b points-to heap-kinds)
  "Return T if REG-A and REG-B may alias under type-based heap classification.

If both roots and kinds are known and the kinds differ, return NIL. Otherwise
stay conservative and return T."
  (let ((root-a (gethash reg-a points-to))
        (root-b (gethash reg-b points-to)))
    (cond
      ((or (null root-a) (null root-b)) t)
      ((eq root-a root-b) t)
      ((opt-tbaa-must-not-alias-p root-a root-b heap-kinds) nil)
      (t t))))

(defun opt-must-alias-p (reg-a reg-b alias-roots)
  "Return T when REG-A and REG-B definitely alias under ALIAS-ROOTS."
  (multiple-value-bind (root-a found-a) (gethash reg-a alias-roots)
    (multiple-value-bind (root-b found-b) (gethash reg-b alias-roots)
      (and found-a found-b (eq root-a root-b)))))

(defun opt-may-alias-p (reg-a reg-b alias-roots &optional type-facts)
  "Return T when REG-A and REG-B may alias under ALIAS-ROOTS.

Unknown roots remain conservative and therefore return T."
  (if (and type-facts (opt-tbaa-must-not-alias-p reg-a reg-b type-facts))
      nil
      (multiple-value-bind (root-a found-a) (gethash reg-a alias-roots)
        (multiple-value-bind (root-b found-b) (gethash reg-b alias-roots)
          (or (not found-a)
              (not found-b)
              (eq root-a root-b))))))

(defun opt-slot-alias-key (obj-reg slot-name alias-roots)
  "Return a canonical slot key for OBJ-REG/SLOT-NAME using ALIAS-ROOTS."
  (multiple-value-bind (root found-p) (gethash obj-reg alias-roots)
    (list :slot (if found-p root obj-reg) slot-name)))

;;; ─── Register-slot rewrite metadata (FR-410) ─────────────────────────────
