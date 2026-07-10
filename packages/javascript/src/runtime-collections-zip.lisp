;;;; packages/javascript/src/runtime-collections-zip.lisp — Iterator.zip helpers

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Iterator.zip / Iterator.zipKeyed built-ins
;;; -----------------------------------------------------------------------

(defun %js-zip-type-error (message)
  (%js-throw message))

(defun %js-zip-primitive-p (value)
  (or (stringp value)
      (numberp value)
      (characterp value)
      (typep value 'boolean)
      (eq value +js-null+)
      (eq value +js-undefined+)))

(defun %js-zip-normalize-iterable (value)
  (cond
    ((%js-zip-primitive-p value)
     (%js-zip-type-error "Iterator.zip expects iterable objects"))
    ((js-set-p value)
     (%js-vec-to-iter (coerce (%js-set-keys value) 'vector)))
    ((%js-vec-p value)
     (%js-vec-to-iter value))
    ((%js-ht-p value)
     (cond
       ((gethash "next" value)
        value)
       ((gethash "@@iterator" value)
        (let ((iter-fn (gethash "@@iterator" value)))
          (unless (functionp iter-fn)
            (%js-zip-type-error "Iterator.zip expects iterable objects"))
          (%js-zip-normalize-iterable (%js-funcall iter-fn))))
       (t
        (%js-zip-type-error "Iterator.zip expects iterable objects"))))
    ((listp value)
     (%js-vec-to-iter (coerce value 'vector)))
    (t
     (%js-zip-type-error "Iterator.zip expects iterable objects"))))

(defun %js-zip-normalize-options (options)
  (cond
    ((or (null options) (eq options +js-undefined+))
     (%js-make-object))
    ((%js-ht-p options)
     options)
    (t
     (%js-zip-type-error "Iterator.zip options must be an object"))))

(defun %js-zip-collect-source-iterators (iterables)
  (let ((outer (%js-zip-normalize-iterable iterables))
        (sources nil))
    (loop
      (multiple-value-bind (item done) (%js-iter-next outer)
        (when done
          (return (nreverse sources)))
        (push (%js-zip-normalize-iterable item) sources)))))

(defun %js-zip-collect-padding (padding-option iter-count)
  (let ((result (make-array iter-count :initial-element +js-undefined+)))
    (when padding-option
      (when (%js-zip-primitive-p padding-option)
        (%js-zip-type-error "Iterator.zip padding must be iterable"))
      (let ((padding-iter (%js-zip-normalize-iterable padding-option)))
        (dotimes (i iter-count)
          (multiple-value-bind (item done) (%js-iter-next padding-iter)
            (when done
              (return))
            (setf (aref result i) item)))))
    result))

(defun %js-zip-collect-keyed-padding (padding-option keys)
  (cond
    ((null padding-option)
     (%js-make-ht))
    ((%js-ht-p padding-option)
     (let ((result (%js-make-ht)))
       (dolist (key keys result)
         (setf (gethash key result) (gethash key padding-option +js-undefined+)))))
    (t
     (%js-zip-type-error "Iterator.zip padding must be an object"))))

(defun %js-zip-read-mode (options)
  (let ((mode (or (gethash "mode" options) "shortest"))
        (padding-option (gethash "padding" options +js-undefined+)))
    (unless (member mode '("shortest" "longest" "strict") :test #'string=)
      (%js-zip-type-error "Iterator.zip mode must be shortest, longest, or strict"))
    (values mode
            (unless (eq padding-option +js-undefined+)
              padding-option))))

(defun %js-iterator-zip (iterables &optional options)
  (let ((options (%js-zip-normalize-options options)))
    (multiple-value-bind (mode padding-option) (%js-zip-read-mode options)
      (let* ((sources (%js-zip-collect-source-iterators iterables))
             (source-count (length sources))
             (padding (when (string= mode "longest")
                        (%js-zip-collect-padding padding-option source-count))))
        (%js-make-cl-iterator
         (lambda ()
           (let ((row (make-array source-count :element-type t
                                  :initial-element +js-undefined+))
                 (all-done-p t)
                 (some-done-p nil))
             (dotimes (i source-count)
               (multiple-value-bind (value done) (%js-iter-next (nth i sources))
                 (if done
                     (progn
                       (setf some-done-p t)
                       (when (string= mode "longest")
                         (setf (aref row i) (aref padding i))))
                     (progn
                       (setf all-done-p nil)
                       (setf (aref row i) value)))))
             (cond
               (all-done-p :done)
               ((and some-done-p (string= mode "strict"))
                (%js-zip-type-error "Iterator.zip strict mode requires equal-length iterables"))
               ((and some-done-p (string= mode "shortest"))
                :done)
               (t
                (cons row nil))))))))))

(defun %js-iterator-zip-keyed (iterables &optional options)
  (let ((options (%js-zip-normalize-options options)))
    (multiple-value-bind (mode padding-option) (%js-zip-read-mode options)
      (unless (%js-ht-p iterables)
        (%js-zip-type-error "Iterator.zipKeyed expects an object"))
      (let ((keys nil)
            (sources nil)
            (padding nil))
        (maphash (lambda (key value)
                   (push key keys)
                   (push (%js-zip-normalize-iterable value) sources))
                 iterables)
        (setf keys (nreverse keys)
              sources (nreverse sources))
        (when (string= mode "longest")
          (setf padding (%js-zip-collect-keyed-padding padding-option keys)))
        (%js-make-cl-iterator
         (lambda ()
           (let ((result (%js-object-create +js-null+))
                 (all-done-p t)
                 (some-done-p nil))
             (loop for key in keys
                   for source in sources
                   do (multiple-value-bind (value done) (%js-iter-next source)
                        (if done
                            (progn
                              (setf some-done-p t)
                              (when (string= mode "longest")
                                (setf (gethash key result)
                                      (gethash key padding +js-undefined+))))
                            (progn
                              (setf all-done-p nil)
                              (setf (gethash key result) value)))))
             (cond
               (all-done-p :done)
               ((and some-done-p (string= mode "strict"))
                (%js-zip-type-error "Iterator.zip strict mode requires equal-length iterables"))
               ((and some-done-p (string= mode "shortest"))
                :done)
               (t
                (cons result nil))))))))))
