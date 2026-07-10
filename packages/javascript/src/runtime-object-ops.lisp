;;;; packages/javascript/src/runtime-object-ops.lisp — JS object algorithms
;;;;
;;;; Object.is / Object.groupBy / structuredClone / destructuring helpers.

(in-package :cl-cc/javascript)

(defun %js-object-is (a b)
  "Object.is — like === but handles NaN and -0."
  (cond
    ((and (%js-nan-p a) (%js-nan-p b)) t)
    ((and (eql a 0.0d0) (eql b -0.0d0)) nil)
    ((and (eql a -0.0d0) (eql b 0.0d0)) nil)
    (t (%js-strict-eq a b))))

(defun %js-object-without-keys (obj keys)
  "Return a copy of OBJ without the given KEYS (vector of strings)."
  (let ((ht (%js-make-ht)))
    (when (%js-ht-p obj)
      (let ((exclude (make-hash-table :test #'equal)))
        (when (%js-vec-p keys)
          (loop for i below (length keys)
                do (setf (gethash (%js-to-string (aref keys i)) exclude) t)))
        (maphash (lambda (k v)
                   (unless (gethash k exclude)
                     (setf (gethash k ht) v)))
                 obj)))
    ht))

(defun %js-object-group-by (iterable key-fn)
  "Object.groupBy(iterable, keyFn): group values into a null-prototype object."
  (let ((ht (%js-object-create +js-null+))
        (index 0))
    (%js-for-of iterable
                (lambda (item)
                  (let* ((k (%js-to-string (%js-funcall key-fn item index)))
                         (bucket (multiple-value-bind (v f) (gethash k ht)
                                   (if f v
                                       (let ((arr (%js-make-array)))
                                         (setf (gethash k ht) arr)
                                         arr)))))
                    (vector-push-extend item bucket)
                    (incf index))))
    ht))

;;; -----------------------------------------------------------------------
;;;  structuredClone — deep copy of a JS value
;;; -----------------------------------------------------------------------

(defun %js-deep-clone (val &optional (seen (make-hash-table)))
  "Deep clone VAL (structuredClone semantics). Handles objects, arrays, Maps, Sets."
  (cond
    ((eq val +js-undefined+) +js-undefined+)
    ((eq val +js-null+)      +js-null+)
    ((eq val +js-nan+)       +js-nan+)
    ((numberp val)           val)
    ((stringp val)           (copy-seq val))
    ((or (eq val t) (eq val nil)) val)
    ((gethash val seen)      (gethash val seen))
    ((%js-vec-p val)
     (let ((clone (make-array (length val) :element-type t :adjustable t :fill-pointer (length val))))
       (setf (gethash val seen) clone)
       (loop for i below (length val)
             do (setf (aref clone i) (%js-deep-clone (aref val i) seen)))
       clone))
    ((js-map-p val)
     (let ((clone (%js-make-map)))
       (setf (gethash val seen) clone)
       (dolist (k (js-map-order val))
         (%js-map-set clone (%js-deep-clone k seen)
                           (%js-deep-clone (gethash k (js-map-ht val) +js-undefined+) seen)))
       clone))
    ((%js-ht-p val)
     (let ((clone (%js-make-ht)))
       (setf (gethash val seen) clone)
       (maphash (lambda (k v)
                  (setf (gethash k clone) (%js-deep-clone v seen)))
                val)
       clone))
    ((typep val 'js-date)
     (make-js-date :ms (js-date-ms val)))
    ((js-regexp-p val)
     (%js-make-regex (js-regexp-source val) (js-regexp-flags val)))
    (t val)))

(defun %js-destructure-array (arr &rest indices-and-defaults)
  "Array-destructuring rest helper."
  (if (and (= (length indices-and-defaults) 2)
           (eq (second indices-and-defaults) :rest))
      (let ((idx (first indices-and-defaults))
            (len (truncate (%js-to-number (%js-get-prop arr "length")))))
        (%js-list-to-array
         (loop for i from idx below len collect (%js-get-prop arr i))))
      (loop for (idx default) on indices-and-defaults by #'cddr
            collect (let ((v (%js-get-prop arr idx)))
                      (if (eq v +js-undefined+) default v)))))

(defun %js-destructure-object (obj &rest keys-and-defaults)
  "Object-destructuring rest helper."
  (if (and keys-and-defaults (eq (first keys-and-defaults) :rest))
      (let ((excluded (rest keys-and-defaults))
            (out (%js-make-object)))
        (dolist (k (coerce (%js-object-keys obj) 'list) out)
          (unless (member k excluded :test #'equal)
            (%js-set-prop out k (%js-get-prop obj k)))))
      (loop for (k default) on keys-and-defaults by #'cddr
            collect (let ((v (%js-get-prop obj k)))
                      (if (eq v +js-undefined+) default v)))))
