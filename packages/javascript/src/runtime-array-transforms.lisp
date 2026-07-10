;;;; packages/javascript/src/runtime-array-transforms.lisp — JS Array transforms and mutation helpers
;;;;
;;;; Load order: after runtime-array-core.lisp and before runtime-array-es2023.lisp.

(in-package :cl-cc/javascript)

;;; Shared sort predicate builder — used by both mutating sort and toSorted.
;;; Handles nil (no compareFn supplied) and +js-undefined+ (passed from JS).
(defun %js-sort-comparator (compare-fn)
  "Build a CL sort predicate from an optional JS compareFn."
  (if (and compare-fn (not (eq compare-fn +js-undefined+)))
      (lambda (a b) (< (%js-funcall compare-fn a b) 0))
      (lambda (a b) (string< (%js-to-string a) (%js-to-string b)))))

(defun %js-array-splice (arr start &optional (delete-count nil delete-count-supplied-p) &rest items)
  "Splice: remove DELETE-COUNT elements at START, insert ITEMS."
  (let* ((n (length arr))
         (s (%js-array-relative-start start n))
         (dc (if (not delete-count-supplied-p)
                 (- n s)
                 (min (max 0 (%js-array-to-integer delete-count)) (- n s))))
         (removed (%js-make-vec dc)))
    ;; collect removed
    (loop for i from s below (+ s dc)
          for j from 0
          do (setf (aref removed j) (aref arr i)))
    ;; build new contents
    (let* ((ni (length items))
           (new-len (+ n ni (- dc)))
           (new-arr (%js-make-vec new-len)))
      (loop for i below s do (setf (aref new-arr i) (aref arr i)))
      (loop for item in items for i from s
            do (setf (aref new-arr i) item))
      (loop for i from (+ s dc) below n
            for j from (+ s ni)
            do (setf (aref new-arr j) (aref arr i)))
      ;; copy new-arr back into arr
      (adjust-array arr new-len :fill-pointer new-len)
      (loop for i below new-len
            do (setf (aref arr i) (aref new-arr i))))
    removed))

(defun %js-array-concat (arr &rest others)
  "Concatenate ARR with OTHERS."
  (let ((result (%js-make-vec)))
    (flet ((extend (x)
             (if (%js-vec-p x)
                 (loop for i below (length x)
                       do (vector-push-extend (aref x i) result))
                 (vector-push-extend x result))))
      (extend arr)
      (dolist (o others) (extend o)))
    result))

(defun %js-array-reverse (arr)
  "Reverse ARR in place; return ARR."
  (let ((n (length arr)))
    (loop for i below (floor n 2)
          do (rotatef (aref arr i) (aref arr (- n 1 i)))))
  arr)

(defun %js-array-sort (arr &optional compare-fn)
  "Sort ARR in place; return ARR."
  (replace arr (stable-sort (copy-seq arr) (%js-sort-comparator compare-fn)))
  arr)

(defun %js-array-flat (arr &optional (depth 1))
  "Flatten ARR up to DEPTH levels."
  (let ((result (%js-make-vec)))
    (labels ((flatten (x d)
               (if (and (%js-vec-p x) (> d 0))
                   (loop for i below (length x)
                         do (flatten (aref x i) (1- d)))
                   (vector-push-extend x result))))
      (loop for i below (length arr)
            do (flatten (aref arr i) depth)))
    result))

(defun %js-array-flat-map (arr fn)
  "Map FN then flatten one level."
  (%js-array-flat (%js-array-map arr fn) 1))

(defun %js-array-fill (arr value &optional (start 0) (end nil))
  "Fill ARR[start..end] with VALUE."
  (let* ((n (length arr))
         (s (%js-array-relative-start start n))
         (e (%js-array-relative-end end n)))
    (loop for i from s below e
          do (setf (aref arr i) value)))
  arr)

(defun %js-array-copy-within (arr target &optional (start 0) (end nil))
  "Copy elements within ARR."
  (let* ((n (length arr))
         (to (%js-array-relative-start target n))
         (from (%js-array-relative-start start n))
         (fin (%js-array-relative-end end n))
         (count (max 0 (min (- fin from) (- n to))))
         (tmp (make-array count)))
    (loop for i below count do (setf (aref tmp i) (aref arr (+ from i))))
    (loop for i below count do (setf (aref arr (+ to i)) (aref tmp i))))
  arr)
