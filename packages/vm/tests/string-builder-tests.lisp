(in-package :cl-cc/test)



(it-sequential "string-builder-bulk-length-clear-and-capacity"
  :timeout
  5
  (let ((builder (cl-cc/vm:make-string-builder :capacity 1)))
    (cl-cc/vm::string-builder-ensure-capacity! builder 128)
    (cl-cc/vm::string-builder-append-string! builder "hello")
    (expect (cl-cc/vm::string-builder-length builder) :to-equal 5)
    (expect (cl-cc/vm:string-builder-finish builder) :to-equal "hello")
    (cl-cc/vm::string-builder-clear! builder)
    (expect (cl-cc/vm::string-builder-length builder) :to-equal 0)
    (cl-cc/vm::string-builder-append-string! builder "world")
    (expect (cl-cc/vm:string-builder-finish builder) :to-equal "world")))

(it-sequential "rope-insert-delete-substring"
  :timeout
  5
  (let* ((base (cl-cc/vm:rope-concat
                (make-string cl-cc/vm::+rope-inline-threshold+ :initial-element #\a)
                (make-string cl-cc/vm::+rope-inline-threshold+ :initial-element #\c)))
         (inserted (cl-cc/vm::rope-insert base cl-cc/vm::+rope-inline-threshold+ "bbb"))
         (window (cl-cc/vm::rope-substring inserted (1- cl-cc/vm::+rope-inline-threshold+)
                                          (+ cl-cc/vm::+rope-inline-threshold+ 4)))
         (deleted (cl-cc/vm::rope-delete inserted cl-cc/vm::+rope-inline-threshold+
                                        (+ cl-cc/vm::+rope-inline-threshold+ 3))))
    (expect (cl-cc/vm:rope-to-string window) :to-equal "abbbc")
    (expect (cl-cc/vm:rope-to-string deleted) :to-equal (cl-cc/vm:rope-to-string base))))
