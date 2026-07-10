;;;; packages/javascript/src/runtime-collections-set.lisp — JS Set built-ins
;;;;
;;;; Set is represented as a js-set struct with a hash-table and an
;;;; insertion-order list (for deterministic for-of iteration). Public Set
;;;; operations canonicalize through the insertion-order values because
;;;; ECMAScript membership uses SameValueZero, which CL hash-table tests cannot
;;;; express exactly for NaN.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Set built-ins (ES2025)
;;; -----------------------------------------------------------------------

;;; JS Set is represented as a struct with:
;;;   HT    — CL hash-table (key → t) for canonical stored values
;;;   ORDER — list of values in insertion order (used by for-of / keys / values)
(defstruct (js-set (:conc-name js-set-))
  (ht    (make-hash-table :test #'equal :size 8) :type hash-table)
  (order nil))

(defun %js-make-set ()
  "Create a new empty ordered JS Set."
  (make-js-set))

;;; Internal helper: copy every key from SRC (a js-set) into TARGET (a js-set).
(defun %js-set-copy-all (target src)
  (dolist (k (js-set-order src))
    (%js-set-add target k)))

(defun %js-set-find-value (s val)
  "Return the stored set value matching VAL by SameValueZero, plus found-p."
  (loop for stored-value in (js-set-order s)
        when (%js-same-value-zero stored-value val)
          return (values stored-value t)
        finally (return (values nil nil))))

(defun %js-set-add (s val)
  (unless (nth-value 1 (%js-set-find-value s val))
    (setf (gethash val (js-set-ht s)) t)
    (setf (js-set-order s) (append (js-set-order s) (list val))))
  s)

(defun %js-set-delete (s val)
  (multiple-value-bind (stored-value found-p) (%js-set-find-value s val)
    (when found-p
      (remhash stored-value (js-set-ht s))
      (setf (js-set-order s) (remove stored-value (js-set-order s) :test #'equal))
      t)))

(defun %js-set-has (s val)
  (nth-value 1 (%js-set-find-value s val)))

(defun %js-set-clear (s)
  (clrhash (js-set-ht s))
  (setf (js-set-order s) nil)
  s)

(defun %js-set-size (s)
  (length (js-set-order s)))

(defun %js-set-keys (s)
  (copy-list (js-set-order s)))

(defun %js-set-entries (s)
  (mapcar (lambda (x) (list x x)) (js-set-order s)))

(defun %js-set-for-each (s fn)
  (dolist (x (js-set-order s))
    (%js-funcall fn x x s))
  +js-undefined+)

(defun %js-set-like-size (obj)
  (cond
      ((js-set-p obj)
       (%js-set-size obj))
      ((%js-ht-p obj)
       (gethash "size" obj 0))))

(defun %js-set-like-has (obj value)
  (cond
      ((js-set-p obj)
       (%js-set-has obj value))
      ((%js-ht-p obj)
       (let ((has (gethash "has" obj)))
         (and has (%js-truthy (%js-funcall has value)))))))

(defun %js-set-like-keys-result-to-list (keys)
  (cond
    ((%js-vec-p keys)
     (coerce keys 'list))
    ((listp keys)
     keys)
    ((js-set-p keys)
     (%js-set-keys keys))
    (t nil)))

(defun %js-set-like-keys (obj)
  (cond
      ((js-set-p obj)
       (%js-set-keys obj))
      ((%js-ht-p obj)
       (let ((keys (gethash "keys" obj)))
         (when keys
           (%js-set-like-keys-result-to-list (%js-funcall keys)))))))

;;; JS Set ops with duck-typed right-hand side (Set or Map-like table)

(defmacro define-js-set-predicate (name stop-condition result-if-stopped default-result)
  `(defun ,name (a b)
     (block check
       (dolist (k (js-set-order a))
         (when ,stop-condition
           (return-from check ,result-if-stopped)))
       ,default-result)))

(defmacro define-js-set-filter-op (name predicate)
  `(defun ,name (a b)
     (let ((result (%js-make-set)))
       (%js-set-copy-all result a)
       (setf (js-set-order result) nil)
       (dolist (k (%js-set-like-keys a) result)
         (when ,predicate (%js-set-add result k)))
       result)))

(defun %js-set-union (a b)
  (let ((result (%js-make-set)))
    (%js-set-copy-all result a)
    (dolist (k (%js-set-like-keys b))
      (%js-set-add result k))
    result))

(define-js-set-filter-op %js-set-intersection
  (%js-set-like-has b k))

(define-js-set-filter-op %js-set-difference
  (not (%js-set-like-has b k)))

(defun %js-set-symmetric-difference (a b)
  (let ((result (%js-make-set)))
    (dolist (k (%js-set-like-keys a))
      (unless (%js-set-like-has b k) (%js-set-add result k)))
    (dolist (k (%js-set-like-keys b))
      (unless (%js-set-has a k) (%js-set-add result k)))
    result))

(define-js-set-predicate %js-set-is-subset-of
  (not (%js-set-like-has b k)) nil t)

(define-js-set-predicate %js-set-is-disjoint-from
  (%js-set-like-has b k) nil t)

(defun %js-set-is-superset-of (a b)
  (dolist (k (%js-set-like-keys b) t)
    (unless (%js-set-has a k)
      (return-from %js-set-is-superset-of nil))))
