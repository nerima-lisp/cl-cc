;;;; packages/javascript/src/runtime-array-from.lisp — JS Array.from helpers
;;;;
;;;; Load order: after runtime-array-core.lisp.

(in-package :cl-cc/javascript)

(defun %js-array-from-map-fn-p (map-fn)
  (and map-fn (not (eq map-fn +js-undefined+)) (not (eq map-fn +js-null+))))

(defun %js-array-from-map-value (map-fn this-arg value index)
  (if (%js-array-from-map-fn-p map-fn)
      (%js-call-with-this this-arg map-fn (list value index))
      value))

(defun %js-array-to-length (value)
  "Coerce VALUE like ECMAScript ToLength."
  (let ((n (%js-to-number value)))
    (cond
      ((or (%js-nan-p n) (<= n 0)) 0)
      ((%js-float-infinity-p n) 9007199254740991)
      (t (min (floor n) 9007199254740991)))))

(defun %js-array-iterable-p (value)
  (or (%js-vec-p value)
      (stringp value)
      (typep value 'js-map)
      (typep value 'js-set)
      (functionp value)
      (and (%js-ht-p value)
           (or (gethash "next" value)
               (gethash "@@iterator" value)))))

(defun %js-array-from (items &optional (map-fn +js-undefined+) (this-arg +js-undefined+))
  "JS Array.from — collects an iterable or array-like object into a fresh array."
  (let ((result (%js-make-vec))
        (index 0))
    (if (%js-array-iterable-p items)
        (%js-for-of
         items
         (lambda (el)
           (vector-push-extend
            (%js-array-from-map-value map-fn this-arg el index)
            result)
           (incf index)))
        (let ((length (%js-array-to-length (%js-get-prop items "length"))))
          (loop for i below length
                do (vector-push-extend
                    (%js-array-from-map-value map-fn this-arg (%js-get-prop items i) i)
                    result))))
    result))

(defun %js-array-from-async (items &optional (map-fn +js-undefined+) (this-arg +js-undefined+))
  "ES2024 Array.fromAsync in the runtime's synchronous Promise model."
  (handler-case
      (let ((result (%js-make-vec))
            (index 0))
        (if (%js-array-iterable-p items)
            (%js-for-of
             items
             (lambda (el)
               (let* ((value (%js-await el))
                      (mapped (%js-array-from-map-value map-fn this-arg value index)))
                 (vector-push-extend (%js-await mapped) result)
                 (incf index))))
            (let ((length (%js-array-to-length (%js-get-prop items "length"))))
              (loop for i below length
                    do (let* ((value (%js-await (%js-get-prop items i)))
                              (mapped (%js-array-from-map-value map-fn this-arg value i)))
                         (vector-push-extend (%js-await mapped) result)))))
        (%js-promise-resolve result))
    (js-exception (c)
      (%js-promise-reject (js-exception-value c)))))

(defun %js-array-is-array (x)
  "True if X is a JS array."
  (%js-vec-p x))
