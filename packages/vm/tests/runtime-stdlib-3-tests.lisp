;;; runtime-stdlib-3-tests.lisp — FR verification tests for runtime-stdlib-3 gaps

(in-package :cl-cc/test)



;; VM format directive at end-of-string has parser bug (FR-965)
;; ~D/~P/~W at EOL all fail with "Unterminated FORMAT directive"
;; Skipped until VM format parser is fixed
(it-sequential "runtime-stdlib-3-external-format-roundtrip"
  (let* ((text "hello")
         (octets (cl-cc/vm:encode-external-format text)))
    (expect (vectorp octets) :to-be-truthy)
    (expect (cl-cc/vm:decode-external-format octets) :to-equal text)))

(it-sequential "runtime-stdlib-3-type-and-numeric-helpers"
  (expect (cl-cc/vm:vm-check-type 3 'integer) :to-equal 3)
  (expect (cl-cc/vm:clamp 7 0 5) :to-equal 5)
  (expect (cl-cc/vm:wrap 6 0 5) :to-equal 1)
  (expect (cl-cc/vm:lerp 10 20 1/2) :to-equal 15)
  (expect (cl-cc/vm:vm-signum -42) :to-equal -1))

(it-sequential "runtime-stdlib-3-environment-and-reader-vars"
  (expect (find :cl-cc cl-cc/vm:*features*) :to-be-truthy)
  (expect (cl-cc/vm:lisp-implementation-type) :to-equal "cl-cc")
  (expect (stringp (cl-cc/vm::short-site-name)) :to-be-truthy)
  (expect (stringp (cl-cc/vm::long-site-name)) :to-be-truthy)
  (expect (= 10 cl-cc/vm:*read-base*) :to-be-truthy)
  (expect (cl-cc/vm:source-location-p
                (cl-cc/vm:make-source-location :pathname #P"x.lisp" :line 1 :column 0)) :to-be-truthy)
  (let ((name "CL_CC_RUNTIME_STDLIB_3_TEST"))
    (cl-cc/vm:setenv name "ok")
    (expect (cl-cc/vm:getenv name) :to-equal "ok")))
