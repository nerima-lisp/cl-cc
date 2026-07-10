;;;; packages/javascript/src/runtime-map.lisp — JS Map core

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Map (ES2015+)
;;; -----------------------------------------------------------------------
;;;
;;; JS Map preserves insertion order and accepts any value as a key.
;;; We represent it as a struct holding insertion order plus a hash-table.
;;; ECMAScript Map key matching uses SameValueZero: NaN matches NaN, +0 and -0
;;; match, and objects use identity.  CL hash-table tests cannot express that
;;; exactly, so public Map operations canonicalize through the insertion-order
;;; keys before touching the hash-table.

(defstruct (js-map (:conc-name js-map-))
  (ht (make-hash-table :test #'equal))   ; key → value
  (order nil))                           ; insertion-order list of keys

(defun %js-map-p (x) (js-map-p x))

(defun %js-map-find-key (m key)
  "Return the stored key in M matching KEY by SameValueZero, plus found-p."
  (loop for stored-key in (js-map-order m)
        when (%js-same-value-zero stored-key key)
          return (values stored-key t)
        finally (return (values nil nil))))

(defun %js-make-map (&optional pairs)
  "Create a JS Map, optionally seeded from an iterable of [key,val] pairs."
  (let ((m (make-js-map)))
    (when (and pairs (not (eq pairs +js-undefined+)) (not (eq pairs +js-null+)))
      (%js-for-of pairs
                  (lambda (pair)
                    (let ((k (%js-get-prop pair 0))
                          (v (%js-get-prop pair 1)))
                      (%js-map-set m k v)))))
    m))

(defun %js-map-set (m key value)
  "Set KEY → VALUE in Map M, preserving insertion order."
  (let ((ht (js-map-ht m)))
    (multiple-value-bind (stored-key found-p) (%js-map-find-key m key)
      (if found-p
          (setf (gethash stored-key ht) value)
          (progn
            (setf (js-map-order m) (nconc (js-map-order m) (list key)))
            (setf (gethash key ht) value)))))
  m)

(defun %js-map-get (m key)
  "Return value at KEY in Map M, or undefined."
  (multiple-value-bind (stored-key found-p) (%js-map-find-key m key)
    (if found-p
        (gethash stored-key (js-map-ht m))
        +js-undefined+)))

(defun %js-map-get-or-insert (m key value)
  "Return existing value at KEY, or insert VALUE and return it."
  (if (%js-map-has m key)
      (%js-map-get m key)
      (progn
        (%js-map-set m key value)
        value)))

(defun %js-map-get-or-insert-computed (m key callback)
  "Return existing value at KEY, or compute and insert a default value."
  (if (%js-map-has m key)
      (%js-map-get m key)
      (let ((callback-value (%js-funcall callback key)))
        (if (%js-map-has m key)
            (%js-map-get m key)
            (progn
              (%js-map-set m key callback-value)
              callback-value)))))

(defun %js-map-has (m key)
  "True if Map M has KEY."
  (nth-value 1 (%js-map-find-key m key)))

(defun %js-map-delete (m key)
  "Remove KEY from Map M; return true if it existed."
  (multiple-value-bind (stored-key found-p) (%js-map-find-key m key)
    (when found-p
      (remhash stored-key (js-map-ht m))
      (setf (js-map-order m)
            (delete stored-key (js-map-order m) :test #'%js-same-value-zero)))
    found-p))

(defun %js-map-clear (m)
  "Remove all entries from Map M."
  (clrhash (js-map-ht m))
  (setf (js-map-order m) nil)
  +js-undefined+)

(defun %js-map-size (m)
  "Return the number of entries in Map M."
  (length (js-map-order m)))

(defmacro %define-js-map-iterator (name docstring &body value-expr)
  "Define a Map iterator that yields VALUE-EXPR (with K and V bound to key/value)."
  `(defun ,name (m)
     ,docstring
     (let ((keys (copy-list (js-map-order m)))
           (ht   (js-map-ht m))
           (i    0))
       (%js-make-cl-iterator
        (lambda ()
          (if (>= i (length keys))
              :done
              (let* ((k (nth i keys))
                     (v (gethash k ht +js-undefined+)))
                (declare (ignorable k v))
                (incf i)
                (cons (progn ,@value-expr) nil))))))))

(%define-js-map-iterator %js-map-keys
  "Return an iterator over Map M's keys in insertion order."
  k)

(%define-js-map-iterator %js-map-values
  "Return an iterator over Map M's values in insertion order."
  v)

(%define-js-map-iterator %js-map-entries
  "Return an iterator over Map M's [key,value] pairs in insertion order."
  (%js-make-array k v))

(defun %js-map-for-each (m fn)
  "Call FN(value, key, map) for each entry in insertion order."
  (dolist (k (js-map-order m))
    (%js-funcall fn (gethash k (js-map-ht m) +js-undefined+) k m))
  +js-undefined+)
