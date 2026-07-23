;;;; tests/unit/emit/wasm-extract-tests.lisp — WASM Function Extraction Tests
;;;;
;;;; Tests for src/emit/wasm-extract.lisp:
;;;; collect-entry-labels, segment-instructions

(in-package :cl-cc/test)



(defun make-entry-labels (&rest labels)
  (let ((entry-labels (make-hash-table :test #'equal)))
    (dolist (label labels entry-labels)
      (setf (gethash label entry-labels) t))))

(defun make-function-instructions (label &rest body)
  (cons (make-vm-label :name label) body))

(defun assert-segment (segment kind &key label body-length)
  (assert-eq kind (car segment))
  (when label
    (assert-equal label (second segment)))
  (when body-length
    (assert-equal body-length (length (third segment)))))

;;; ─── collect-entry-labels ─────────────────────────────────────────────────────

(it-sequential "extract-entry-labels-nil-input-returns-empty-hash-table"
  (let ((ht-nil (cl-cc/codegen::collect-entry-labels nil)))
    (expect (hash-table-p ht-nil) :to-be-truthy)
    (expect (hash-table-count ht-nil) :to-equal 0)))

(it-sequential "extract-entry-labels-non-closure-instructions-return-empty"
  (let* ((instrs (list (make-vm-const :dst :r0 :value 42)
                       (make-vm-ret :reg :r0)))
         (ht (cl-cc/codegen::collect-entry-labels instrs)))
    (expect (hash-table-count ht) :to-equal 0)))

(it-sequential "extract-entry-labels-single-entry closure"
  (destructuring-bind (instrs label) (list (list (make-vm-closure :dst :r0 :label "fn1" :params '(:r1) :captured nil)
                            (make-vm-ret :reg :r0)) "fn1")
    (let ((ht (cl-cc/codegen::collect-entry-labels instrs)))
    (expect (hash-table-count ht) :to-equal 1)
    (expect (gethash label ht) :to-be-truthy))))

(it-sequential "extract-entry-labels-single-entry func-ref"
  (destructuring-bind (instrs label) (list (list (make-vm-func-ref :dst :r0 :label "fn2")) "fn2")
    (let ((ht (cl-cc/codegen::collect-entry-labels instrs)))
    (expect (hash-table-count ht) :to-equal 1)
    (expect (gethash label ht) :to-be-truthy))))

(it-sequential "extract-entry-labels-mixed"
  (let* ((instrs (list (make-vm-closure :dst :r0 :label "fn-a"
                                        :params '(:r1) :captured nil)
                       (make-vm-func-ref :dst :r1 :label "fn-b")))
         (ht (cl-cc/codegen::collect-entry-labels instrs)))
    (expect (hash-table-count ht) :to-equal 2)
    (expect (gethash "fn-a" ht) :to-be-truthy)
    (expect (gethash "fn-b" ht) :to-be-truthy)))

(it-sequential "extract-entry-labels-dedup"
  (let* ((instrs (list (make-vm-closure :dst :r0 :label "same"
                                        :params nil :captured nil)
                       (make-vm-func-ref :dst :r1 :label "same")))
         (ht (cl-cc/codegen::collect-entry-labels instrs)))
    (expect (hash-table-count ht) :to-equal 1)))

(it-sequential "extract-entry-labels-ignores-register-function"
  (let* ((instrs (list (cl-cc:make-vm-register-function :name 'foo :src :r0)))
         (ht (cl-cc/codegen::collect-entry-labels instrs)))
    (expect (hash-table-count ht) :to-equal 0)))

;;; ─── segment-instructions ─────────────────────────────────────────────────────

(it-sequential "segment-empty-instructions"
  (let ((entry-labels (make-entry-labels)))
    (expect (cl-cc/codegen::segment-instructions nil entry-labels) :to-be-null)))

(it-sequential "segment-all-toplevel"
  (let ((entry-labels (make-entry-labels))
        (instrs (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-ret :reg :r0))))
    (let ((segs (cl-cc/codegen::segment-instructions instrs entry-labels)))
      (expect (length segs) :to-equal 1)
      (assert-segment (first segs) :toplevel)
      (expect (length (cdr (first segs))) :to-equal 2))))

(it-sequential "segment-one-function"
  (let ((entry-labels (make-entry-labels "fn1")))
    (let* ((body-inst (make-vm-const :dst :r0 :value 99))
           (ret (make-vm-ret :reg :r0))
           (instrs (make-function-instructions "fn1" body-inst ret)))
      (let ((segs (cl-cc/codegen::segment-instructions instrs entry-labels)))
        (expect (length segs) :to-equal 1)
        (assert-segment (first segs) :function :label "fn1" :body-length 3)))))

(it-sequential "segment-mixed-ordering toplevel-first"
  (destructuring-bind (toplevel-first expected-car1 expected-car2) (list t :toplevel :function)
    (let ((entry-labels (make-entry-labels "fn1")))
    (let* ((top (make-vm-const :dst :r0 :value 0))
           (lbl (make-vm-label :name "fn1"))
           (ret (make-vm-ret :reg :r0))
           (instrs (if toplevel-first (list top lbl ret) (list lbl ret top))))
      (let ((segs (cl-cc/codegen::segment-instructions instrs entry-labels)))
        (expect (length segs) :to-equal 2)
        (assert-segment (first segs) expected-car1)
        (assert-segment (second segs) expected-car2))))))

(it-sequential "segment-mixed-ordering function-first"
  (destructuring-bind (toplevel-first expected-car1 expected-car2) (list nil :function :toplevel)
    (let ((entry-labels (make-entry-labels "fn1")))
    (let* ((top (make-vm-const :dst :r0 :value 0))
           (lbl (make-vm-label :name "fn1"))
           (ret (make-vm-ret :reg :r0))
           (instrs (if toplevel-first (list top lbl ret) (list lbl ret top))))
      (let ((segs (cl-cc/codegen::segment-instructions instrs entry-labels)))
        (expect (length segs) :to-equal 2)
        (assert-segment (first segs) expected-car1)
        (assert-segment (second segs) expected-car2))))))

(it-sequential "segment-two-functions"
  (let ((entry-labels (make-entry-labels "fn-a" "fn-b")))
    (let* ((ret-a (make-vm-ret :reg :r0))
           (ret-b (make-vm-ret :reg :r1))
           (instrs (append (make-function-instructions "fn-a" ret-a)
                           (make-function-instructions "fn-b" ret-b))))
      (let ((segs (cl-cc/codegen::segment-instructions instrs entry-labels)))
        (expect (length segs) :to-equal 2)
        (assert-segment (first segs) :function :label "fn-a")
        (assert-segment (second segs) :function :label "fn-b")))))

(it-sequential "segment-non-entry-label-stays-toplevel"
  (let ((entry-labels (make-entry-labels)))
    ;; "loop" is NOT an entry label
    (let* ((lbl (make-vm-label :name "loop"))
           (inst (make-vm-const :dst :r0 :value 1))
           (instrs (list lbl inst)))
      (let ((segs (cl-cc/codegen::segment-instructions instrs entry-labels)))
        (expect (length segs) :to-equal 1)
        (assert-segment (first segs) :toplevel)))))
