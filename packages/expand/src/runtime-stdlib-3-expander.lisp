;;; runtime-stdlib-3-expander.lisp — FR-935 proclamation recording

(in-package :cl-cc/expand)

(defun %global-proclamation-bucket (kind)
  "Return KIND's proclamation table, creating it on first use."
  (or (gethash kind *global-proclamations*)
      (setf (gethash kind *global-proclamations*) (make-hash-table :test #'eq))))

(defun %record-global-proclamation (kind name value)
  "Record proclamation KIND for NAME with VALUE in *GLOBAL-PROCLAMATIONS*."
  (when (symbolp name)
    (setf (gethash name (%global-proclamation-bucket kind)) value)))

(defun %record-declaim-type-clause (clause)
  "Record (TYPE type-spec var*) proclamations."
  (when (and (consp clause) (eq (car clause) 'type) (cddr clause))
    (let ((type-spec (second clause)))
      (dolist (name (cddr clause))
        (%record-global-proclamation 'type name type-spec)))))

(defun %record-declaim-ftype-clause (clause)
  "Record (FTYPE function-type name*) proclamations."
  (when (and (consp clause) (eq (car clause) 'ftype) (cddr clause))
    (let ((type-spec (second clause)))
      (dolist (name (cddr clause))
        (%record-global-proclamation 'ftype name type-spec)))))

(defun %record-declaim-special-clause (clause)
  "Record (SPECIAL var*) proclamations."
  (when (and (consp clause) (eq (car clause) 'special))
    (dolist (name (cdr clause))
      (%record-global-proclamation 'special name t))))

(defun %record-runtime-stdlib-3-declaim-clause (clause)
  "Record every DECLAIM clause whose side effects are tracked by the expander."
  (%record-declaim-inline-clause clause)
  (%record-declaim-optimize-clause clause)
  (%record-declaim-type-clause clause)
  (%record-declaim-ftype-clause clause)
  (%record-declaim-special-clause clause))

(register-macro 'declaim
  (lambda (form env)
    (declare (ignore env))
    (dolist (clause (cdr form))
      (%record-runtime-stdlib-3-declaim-clause clause))
    nil))

(export '(*global-proclamations*))

;;; ── Host bridges for PROCLAIM ────────────────────────────────────────────────
;;;
;;; The stdlib defines PROCLAIM as guest code that calls these recorders by
;;; package-qualified name (see stdlib-source.lisp). The VM can only call a host
;;; function that has been registered as a bridge, so without this PROCLAIM
;;; compiled to a call of an undefined function. Registration lives here rather
;;; than in the VM's own bridge table because cl-cc-vm loads before this system
;;; and cannot name these functions.
(eval-when (:load-toplevel :execute)
  (when (fboundp 'cl-cc/vm:vm-register-host-bridge)
    (dolist (entry (list (cons '%record-declaim-inline-clause
                               #'%record-declaim-inline-clause)
                         (cons '%record-declaim-optimize-clause
                               #'%record-declaim-optimize-clause)
                         (cons '%record-declaim-type-clause
                               #'%record-declaim-type-clause)
                         (cons '%record-declaim-ftype-clause
                               #'%record-declaim-ftype-clause)
                         (cons '%record-declaim-special-clause
                               #'%record-declaim-special-clause)))
      (cl-cc/vm:vm-register-host-bridge (car entry) (cdr entry)))))
