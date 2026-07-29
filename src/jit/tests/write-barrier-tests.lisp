(in-package :cl-cc/test)

(defun make-write-barrier-test-heap ()
  (cl-cc/runtime:make-rt-heap :young-size 16 :old-size 16))

(it-sequential
  "jit-write-barrier-generation-boundaries"
  (let* ((heap (make-write-barrier-test-heap))
         (young-base (cl-cc/runtime:rt-heap-young-from-base heap))
         (young-end (+ young-base (cl-cc/runtime:rt-heap-young-semi-size heap)))
         (old-base (cl-cc/runtime:rt-heap-old-base heap))
         (old-end (+ old-base (cl-cc/runtime:rt-heap-old-size heap))))
    (expect (cl-cc/jit:in-young-generation-p heap young-base) :to-be-truthy)
    (expect (not (cl-cc/jit:in-young-generation-p heap young-end)) :to-be-truthy)
    (expect (cl-cc/jit:in-old-generation-p heap old-base) :to-be-truthy)
    (expect (not (cl-cc/jit:in-old-generation-p heap old-end)) :to-be-truthy)))

(it-sequential
  "jit-write-barrier-reference-quadrants-and-representations"
  (let* ((heap (make-write-barrier-test-heap))
         (young (cl-cc/runtime:rt-heap-young-from-base heap))
         (old (cl-cc/runtime:rt-heap-old-base heap))
         (boxed-young (cl-cc/runtime:encode-pointer young cl-cc/runtime:+tag-object+))
         (boxed-old (cl-cc/runtime:encode-pointer old cl-cc/runtime:+tag-object+)))
    (expect (cl-cc/jit:old-to-young-reference-p heap old young) :to-be-truthy)
    (expect (not (cl-cc/jit:old-to-young-reference-p heap old old)) :to-be-truthy)
    (expect
      (not (cl-cc/jit:old-to-young-reference-p heap young young))
      :to-be-truthy)
    (expect (not (cl-cc/jit:old-to-young-reference-p heap young old)) :to-be-truthy)
    (expect
      (cl-cc/jit:old-to-young-reference-p heap boxed-old boxed-young)
      :to-be-truthy)
    (expect
      (not (cl-cc/jit:in-young-generation-p heap (cl-cc/runtime:encode-fixnum 1)))
      :to-be-truthy)
    (expect (not (cl-cc/jit:in-young-generation-p heap :immediate)) :to-be-truthy)
    (expect (not (cl-cc/jit:in-old-generation-p heap nil)) :to-be-truthy)))

(it-sequential
  "jit-write-barrier-tracks-minor-flips-per-heap"
  (let* ((heap-a (make-write-barrier-test-heap))
         (heap-b (make-write-barrier-test-heap))
         (old-from (cl-cc/runtime:rt-heap-young-from-base heap-a))
         (new-from (cl-cc/runtime:rt-heap-young-to-base heap-a))
         (heap-b-young (cl-cc/runtime:rt-heap-young-from-base heap-b)))
    (expect (cl-cc/jit:in-young-generation-p heap-a old-from) :to-be-truthy)
    (expect (not (cl-cc/jit:in-young-generation-p heap-a new-from)) :to-be-truthy)
    (cl-cc/runtime:rt-gc-minor-collect heap-a)
    (expect (not (cl-cc/jit:in-young-generation-p heap-a old-from)) :to-be-truthy)
    (expect (cl-cc/jit:in-young-generation-p heap-a new-from) :to-be-truthy)
    (expect (cl-cc/jit:in-young-generation-p heap-b heap-b-young) :to-be-truthy)
    (expect
      (not (cl-cc/jit:in-young-generation-p heap-a heap-b-young))
      :to-be-truthy)))

(it-sequential
  "jit-write-barrier-emission-is-explicitly-unsupported"
  (let ((condition
        (handler-case (progn
            (cl-cc/jit:emit-write-barrier nil 0)
            nil)
          (error (error)
            error))))
    (expect (typep condition (quote error)) :to-be-truthy)
    (expect
      (search "unsupported" (princ-to-string condition) :test (function char-equal))
      :to-be-truthy)))
