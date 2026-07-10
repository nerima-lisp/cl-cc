;;;; packages/javascript/src/runtime-weak-collections.lisp — JS WeakMap, WeakSet, WeakRef, FinalizationRegistry

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  WeakMap (ES2015+)
;;; -----------------------------------------------------------------------
;;;
;;; CL has no native weak references portable across implementations, so
;;; WeakMap is backed by a normal hash table using #'eq (identity equality).
;;; This gives correct JS semantics for object keys; primitive keys would be
;;; invalid per spec but we accept them silently for robustness.

(defstruct (js-weak-map (:conc-name js-weak-map-))
  (ht (make-hash-table :test #'eq)))

(defun %js-weak-map-p (x) (js-weak-map-p x))

(defun %js-make-weak-map () (make-js-weak-map))

(defun %js-weak-map-set (m key value)
  (setf (gethash key (js-weak-map-ht m)) value)
  m)

(defun %js-weak-map-get (m key)
  (multiple-value-bind (v f) (gethash key (js-weak-map-ht m))
    (if f v +js-undefined+)))

(defun %js-weak-map-get-or-insert (m key value)
  "Return existing value at KEY, or insert VALUE and return it."
  (multiple-value-bind (existing found-p) (gethash key (js-weak-map-ht m))
    (if found-p
        existing
        (progn
          (%js-weak-map-set m key value)
          value))))

(defun %js-weak-map-get-or-insert-computed (m key callback)
  "Return existing value at KEY, or compute and insert a default value."
  (multiple-value-bind (existing found-p) (gethash key (js-weak-map-ht m))
    (if found-p
        existing
        (let ((callback-value (%js-funcall callback key)))
          (multiple-value-bind (existing-after found-after)
              (gethash key (js-weak-map-ht m))
            (if found-after
                existing-after
                (progn
                  (%js-weak-map-set m key callback-value)
                  callback-value)))))))

(defun %js-weak-map-has (m key)
  (nth-value 1 (gethash key (js-weak-map-ht m))))

(defun %js-weak-map-delete (m key)
  (multiple-value-bind (v f) (gethash key (js-weak-map-ht m))
    (declare (ignore v))
    (when f (remhash key (js-weak-map-ht m)))
    f))

;;; -----------------------------------------------------------------------
;;;  WeakSet (ES2015+)
;;; -----------------------------------------------------------------------

(defstruct (js-weak-set (:conc-name js-weak-set-))
  (ht (make-hash-table :test #'eq)))

(defun %js-weak-set-p (x) (js-weak-set-p x))

(defun %js-make-weak-set () (make-js-weak-set))

(defun %js-weak-set-add (s value)
  (setf (gethash value (js-weak-set-ht s)) t)
  s)

(defun %js-weak-set-has (s value)
  (nth-value 1 (gethash value (js-weak-set-ht s))))

(defun %js-weak-set-delete (s value)
  (multiple-value-bind (v f) (gethash value (js-weak-set-ht s))
    (declare (ignore v))
    (when f (remhash value (js-weak-set-ht s)))
    f))

;;; -----------------------------------------------------------------------
;;;  WeakRef (ES2021) — deterministic host-backed model
;;; -----------------------------------------------------------------------

(defstruct (js-weak-ref (:conc-name js-weak-ref-))
  target)

(defun %js-make-weak-ref (target)
  "Create a WeakRef.
Portable CL does not expose ECMAScript-style weak reachability guarantees here,
so deref remains deterministic while the wrapper is alive."
  (make-js-weak-ref :target target))

(defun %js-weak-ref-deref (wr) (js-weak-ref-target wr))

;;; Prelude constructor values — plain function references wrapped in
;;; defparameter so seed-js-runtime-globals picks them up (boundp = t)
;;; and the compiler emits vm-get-global without an "Unbound variable" error.
(defparameter *js-weak-map-global* #'%js-make-weak-map)
(defparameter *js-weak-set-global* #'%js-make-weak-set)

;;; -----------------------------------------------------------------------
;;;  FinalizationRegistry (ES2021) — deterministic registration model
;;; -----------------------------------------------------------------------

(defstruct (js-finalization-registry (:conc-name js-finreg-))
  callback
  (registrations nil))

(defstruct (js-finalization-registration (:conc-name js-finreg-entry-))
  target
  held-value
  unregister-token)

(defun %js-make-finalization-registry (callback)
  (make-js-finalization-registry :callback callback))

(defun %js-finreg-register (reg target held-value &optional (unregister-token +js-undefined+))
  "Register TARGET with HELD-VALUE.
Cleanup callbacks are not run by this portable runtime, but registrations are
tracked so unregister has observable ECMAScript-compatible state."
  (push (make-js-finalization-registration
         :target target
         :held-value held-value
         :unregister-token unregister-token)
        (js-finreg-registrations reg))
  +js-undefined+)

(defun %js-finreg-unregister (reg token)
  "Remove all registrations associated with TOKEN, returning true if any existed."
  (let ((removed nil))
    (setf (js-finreg-registrations reg)
          (remove-if (lambda (entry)
                       (let ((stored (js-finreg-entry-unregister-token entry)))
                         (when (and (not (eq stored +js-undefined+))
                                    (%js-same-value-zero stored token))
                           (setf removed t)
                           t)))
                     (js-finreg-registrations reg)))
    removed))
