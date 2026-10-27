(in-package :cl-cc/test)



(defun assert-pmap-entry (map key expected &key (test #'equal))
  (multiple-value-bind (value found-p) (cl-cc/vm:pmap-get map key)
    (expect found-p :to-be-truthy)
    (expect (funcall test expected value) :to-be-truthy)))

