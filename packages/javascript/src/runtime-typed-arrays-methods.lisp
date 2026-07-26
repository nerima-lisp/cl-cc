;;;; packages/javascript/src/runtime-typed-arrays-methods.lisp — ES2023+ TypedArray methods
;;;;
;;;; ES2023 non-mutating methods (toReversed, toSorted, with, at, findLast, etc.)
;;;; *js-typed-array-methods* dispatch table (references fns from both files)
;;;;
;;;; Load order: after runtime-typed-arrays.lisp (needs struct, coerce helpers,
;;;; %js-ta-* bulk methods defined there).

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Method dispatch table for TypedArrays
;;; -----------------------------------------------------------------------

;;; ─── Shared helpers ─────────────────────────────────────────────────────────

(defun %js-ta-clone-with-buffer (ta new-buf)
  "Return a new TypedArray with the same type/element-size as TA but backed by NEW-BUF."
  (make-js-typed-array :type-name (js-ta-type-name ta)
                        :element-size (js-ta-element-size ta)
                        :buffer new-buf :byte-offset 0 :length (length new-buf)))

;;; ─── Core TypedArray prototype methods ──────────────────────────────────────

(defun %js-ta-set-from (ta source &optional (offset 0))
  "TypedArray.prototype.set(array, offset)."
  (let ((off (truncate (%js-to-number offset))))
    (cond
      ((js-typed-array-p source)
       (dotimes (i (js-ta-length source))
         (when (< (+ off i) (js-ta-length ta))
           (setf (aref (js-ta-buffer ta) (+ off i))
                 (%js-ta-coerce-element (js-ta-type-name ta) (aref (js-ta-buffer source) i))))))
      ((%js-vec-p source)
       (dotimes (i (length source))
         (when (< (+ off i) (js-ta-length ta))
           (setf (aref (js-ta-buffer ta) (+ off i))
                 (%js-ta-coerce-element (js-ta-type-name ta) (aref source i)))))))
    ta))

(defun %js-ta-subarray (ta begin &optional end)
  "TypedArray.prototype.subarray(begin, end)."
  (let* ((n (js-ta-length ta))
         (b (if (< begin 0) (max 0 (+ n begin)) (min begin n)))
         (e (if (null end) n (if (< end 0) (max 0 (+ n end)) (min end n))))
         (new-len (max 0 (- e b)))
         (new-buf (subseq (js-ta-buffer ta) b (+ b new-len))))
    (make-js-typed-array :type-name (js-ta-type-name ta)
                         :element-size (js-ta-element-size ta)
                         :buffer new-buf
                         :byte-offset (* b (js-ta-element-size ta))
                         :length new-len)))

(defun %js-ta-slice (ta &optional (begin 0) end)
  "TypedArray.prototype.slice — returns a copy."
  (%js-ta-subarray ta begin end))

(defun %js-ta-fill (ta value &optional (begin 0) end)
  "TypedArray.prototype.fill(value, begin, end)."
  (let* ((n (js-ta-length ta))
         (b (if (< begin 0) (max 0 (+ n begin)) (min begin n)))
         (e (if (null end) n (if (< end 0) (max 0 (+ n end)) (min end n))))
         (coerced (%js-ta-coerce-element (js-ta-type-name ta) value)))
    (loop for i from b below e
          do (setf (aref (js-ta-buffer ta) i) coerced)))
  ta)

(defun %js-ta-to-array (ta)
  "Convert TypedArray to plain JS array."
  (let* ((n (js-ta-length ta))
         (result (make-array n :element-type t :adjustable t :fill-pointer n)))
    (dotimes (i n)
      (setf (aref result i) (coerce (aref (js-ta-buffer ta) i) 'double-float)))
    result))

(defun %js-ta-index-of (ta search-element &optional (from-index 0))
  "TypedArray.prototype.indexOf."
  (let* ((target (%js-ta-coerce-element (js-ta-type-name ta) search-element))
         (n (js-ta-length ta))
         (start (%js-array-relative-start from-index n)))
    (loop for i from start below n
          ;; Strict equality, not (=): indexOf must never match NaN, and a bare
          ;; (=) against one raises FLOATING-POINT-INVALID-OPERATION where the
          ;; :INVALID trap is enabled — SBCL's default on x86-64.
          when (%js-strict-eq (aref (js-ta-buffer ta) i) target) return i
          finally (return -1))))

(defun %js-ta-includes (ta search-element &optional (from-index 0))
  "TypedArray.prototype.includes."
  (let* ((target (%js-ta-coerce-element (js-ta-type-name ta) search-element))
         (n (js-ta-length ta))
         (start (%js-array-relative-start from-index n)))
    (loop for i from start below n
          when (%js-same-value-zero (aref (js-ta-buffer ta) i) target) return t
          finally (return nil))))

(defun %js-ta-join (ta &optional (sep ","))
  "TypedArray.prototype.join."
  (let ((sep-str (%js-to-string sep)))
    (with-output-to-string (out)
      (dotimes (i (js-ta-length ta))
        (when (> i 0) (write-string sep-str out))
        (write-string (format nil "~A" (aref (js-ta-buffer ta) i)) out)))))

(defun %js-ta-for-each (ta fn)
  "TypedArray.prototype.forEach."
  (dotimes (i (js-ta-length ta))
    (%js-funcall fn (coerce (aref (js-ta-buffer ta) i) 'double-float) i ta))
  +js-undefined+)

(defun %js-ta-map (ta fn)
  "TypedArray.prototype.map."
  (let* ((n (js-ta-length ta))
         (result (%js-make-typed-array (js-ta-type-name ta) n)))
    (dotimes (i n)
      (%js-ta-set result i (%js-funcall fn (coerce (aref (js-ta-buffer ta) i) 'double-float) i ta)))
    result))

(defun %js-ta-filter (ta fn)
  "TypedArray.prototype.filter."
  (let ((results nil))
    (dotimes (i (js-ta-length ta))
      (let ((v (coerce (aref (js-ta-buffer ta) i) 'double-float)))
        (when (%js-truthy (%js-funcall fn v i ta))
          (push v results))))
    (let* ((filtered (nreverse results))
           (result (%js-make-typed-array (js-ta-type-name ta) (length filtered))))
      (loop for v in filtered for i from 0
            do (%js-ta-set result i v))
      result)))

(defun %js-ta-reduce (ta fn &optional (init +js-undefined+))
  "TypedArray.prototype.reduce."
  (let ((acc init) (first-p (eq init +js-undefined+)))
    (dotimes (i (js-ta-length ta))
      (let ((v (coerce (aref (js-ta-buffer ta) i) 'double-float)))
        (if first-p
            (setf acc v first-p nil)
            (setf acc (%js-funcall fn acc v i ta)))))
    acc))

;;; ES2023 non-mutating methods, iterators, and method table live in
;;; runtime-typed-arrays-methods-es2023.lisp.
