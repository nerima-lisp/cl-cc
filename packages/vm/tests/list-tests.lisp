;;;; tests/unit/vm/list-tests.lisp — VM List Instruction Tests
;;;
;;; Tests for execute-instruction on list operations, named accessors,
;;; destructive ops, association lists, coercions, arrays, and bit arrays.

(in-package :cl-cc/test)

;;; ─── make-list ──────────────────────────────────────────────────────────────

(it-sequential "vm-list-make-list"
  (with-test-vm (s (1 3))
    (exec1 (cl-cc:make-vm-make-list :dst 0 :size 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal '(nil nil nil)))
  (with-test-vm (s (1 0))
    (exec1 (cl-cc:make-vm-make-list :dst 0 :size 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-be-null)))

;;; ─── length / reverse / append ──────────────────────────────────────────────

(it-sequential "vm-list-unary-src-dst-ops length"
  (destructuring-bind (constructor input expected) (list #'cl-cc:make-vm-length '(a b c d) 4)
    (with-test-vm (s (1 input))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-list-unary-src-dst-ops reverse"
  (destructuring-bind (constructor input expected) (list #'cl-cc:make-vm-reverse '(1 2 3) '(3 2 1))
    (with-test-vm (s (1 input))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-list-append-two"
  (with-test-vm (s (1 '(a b)) (2 '(c d)))
    (exec1 (cl-cc:make-vm-append :dst 0 :src1 1 :src2 2) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal '(a b c d))))

;;; ─── member / nth / nthcdr ─────────────────────────────────────────────────

(it-sequential "vm-list-member-hit-miss hit"
  (destructuring-bind (item lst expected) (list 'b '(a b c) '(b c))
    (with-test-vm (s (1 item) (2 lst))
    (exec1 (cl-cc:make-vm-member :dst 0 :item 1 :list 2) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-list-member-hit-miss miss"
  (destructuring-bind (item lst expected) (list 'z '(a b c) nil)
    (with-test-vm (s (1 item) (2 lst))
    (exec1 (cl-cc:make-vm-member :dst 0 :item 1 :list 2) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-list-indexed-access-ops nth"
  (destructuring-bind (constructor expected assert-fn) (list #'cl-cc:make-vm-nth 'c (lambda (expected actual) (expect actual :to-be expected)))
    (with-test-vm (s (1 2) (2 '(a b c d)))
    (exec1 (funcall constructor :dst 0 :index 1 :list 2) s)
    (funcall assert-fn expected (cl-cc:vm-reg-get s 0)))))

(it-sequential "vm-list-indexed-access-ops nthcdr"
  (destructuring-bind (constructor expected assert-fn) (list #'cl-cc:make-vm-nthcdr '(c d) (lambda (expected actual) (expect actual :to-equal expected)))
    (with-test-vm (s (1 2) (2 '(a b c d)))
    (exec1 (funcall constructor :dst 0 :index 1 :list 2) s)
    (funcall assert-fn expected (cl-cc:vm-reg-get s 0)))))

;;; ─── Named accessors ───────────────────────────────────────────────────────

(it-sequential "vm-list-named-accessors first"
  (destructuring-bind (constructor input expected) (list #'cl-cc:make-vm-first '(10 20 30 40 50) 10)
    (with-test-vm (s (1 input))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-list-named-accessors second"
  (destructuring-bind (constructor input expected) (list #'cl-cc:make-vm-second '(10 20 30 40 50) 20)
    (with-test-vm (s (1 input))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-list-named-accessors third"
  (destructuring-bind (constructor input expected) (list #'cl-cc:make-vm-third '(10 20 30 40 50) 30)
    (with-test-vm (s (1 input))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-list-named-accessors fourth"
  (destructuring-bind (constructor input expected) (list #'cl-cc:make-vm-fourth '(10 20 30 40 50) 40)
    (with-test-vm (s (1 input))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-list-named-accessors fifth"
  (destructuring-bind (constructor input expected) (list #'cl-cc:make-vm-fifth '(10 20 30 40 50) 50)
    (with-test-vm (s (1 input))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-list-named-accessors rest"
  (destructuring-bind (constructor input expected) (list #'cl-cc:make-vm-rest '(a b c) '(b c))
    (with-test-vm (s (1 input))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-list-named-accessors last"
  (destructuring-bind (constructor input expected) (list #'cl-cc:make-vm-last '(a b c) '(c))
    (with-test-vm (s (1 input))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-list-named-accessors butlast"
  (destructuring-bind (constructor input expected) (list #'cl-cc:make-vm-butlast '(a b c) '(a b))
    (with-test-vm (s (1 input))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

;;; ─── Destructive + extended ops ─────────────────────────────────────────────

(it-sequential "vm-list-extended-unary-ops nreverse"
  (destructuring-bind (constructor input expected assert-fn) (list #'cl-cc:make-vm-nreverse (list 1 2 3) '(3 2 1) (lambda (expected actual) (expect actual :to-equal expected)))
    (with-test-vm (s (1 input))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (funcall assert-fn expected (cl-cc:vm-reg-get s 0)))))

(it-sequential "vm-list-extended-unary-ops list-length"
  (destructuring-bind (constructor input expected assert-fn) (list #'cl-cc:make-vm-list-length '(x y z) 3 (lambda (expected actual) (expect (= expected actual) :to-be-truthy)))
    (with-test-vm (s (1 input))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (funcall assert-fn expected (cl-cc:vm-reg-get s 0)))))

(it-sequential "vm-list-empty-predicates endp/nil"
  (destructuring-bind (constructor value expected) (list #'cl-cc:make-vm-endp nil 1)
    (with-test-vm (s (1 value))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (= expected (cl-cc:vm-reg-get s 0)) :to-be-truthy))))

(it-sequential "vm-list-empty-predicates endp/non-empty"
  (destructuring-bind (constructor value expected) (list #'cl-cc:make-vm-endp '(a) 0)
    (with-test-vm (s (1 value))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (= expected (cl-cc:vm-reg-get s 0)) :to-be-truthy))))

(it-sequential "vm-list-empty-predicates null/nil"
  (destructuring-bind (constructor value expected) (list #'cl-cc:make-vm-null nil 1)
    (with-test-vm (s (1 value))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (= expected (cl-cc:vm-reg-get s 0)) :to-be-truthy))))

(it-sequential "vm-list-empty-predicates null/non-nil"
  (destructuring-bind (constructor value expected) (list #'cl-cc:make-vm-null 42 0)
    (with-test-vm (s (1 value))
    (exec1 (funcall constructor :dst 0 :src 1) s)
    (expect (= expected (cl-cc:vm-reg-get s 0)) :to-be-truthy))))

(it-sequential "vm-list-push-and-pop"
  (with-test-vm (s (1 'x) (2 '(a b)))
    (exec1 (cl-cc:make-vm-push :dst 0 :item 1 :list 2) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal '(x a b)))
  (with-test-vm (s (1 '(first second third)))
    (exec1 (cl-cc:make-vm-pop :dst 0 :list 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-be 'first)))

(it-sequential "vm-cons-returns-fresh-cells"
  (let ((s (make-test-vm)))
    (cl-cc/vm::vm-clear-hash-cons-table)
    (cl-cc:vm-reg-set s 1 'a)
    (cl-cc:vm-reg-set s 2 'b)
    (exec1 (cl-cc:make-vm-cons :dst 0 :car-src 1 :cdr-src 2) s)
    (exec1 (cl-cc:make-vm-cons :dst 3 :car-src 1 :cdr-src 2) s)
    (expect (eq (cl-cc:vm-reg-get s 0)
                      (cl-cc:vm-reg-get s 3)) :to-be-falsy)
    (expect (cl-cc:vm-reg-get s 3) :to-equal (cl-cc:vm-reg-get s 0))))

(it-sequential "vm-hash-cons-behavior reuses-identical"
  (destructuring-bind (clear-between-p) (list nil)
    (cl-cc/vm::vm-clear-hash-cons-table) (let ((c1 (cl-cc/vm::vm-hash-cons 'a 'b)))
    (when clear-between-p (cl-cc/vm::vm-clear-hash-cons-table))
    (let ((c2 (cl-cc/vm::vm-hash-cons 'a 'b)))
      (expect c2 :to-equal c1)
      (if clear-between-p
          (expect (eq c1 c2) :to-be-falsy)
          (expect (eq c1 c2) :to-be-truthy))))))

(it-sequential "vm-hash-cons-behavior clear-breaks-reuse"
  (destructuring-bind (clear-between-p) (list t)
    (cl-cc/vm::vm-clear-hash-cons-table) (let ((c1 (cl-cc/vm::vm-hash-cons 'a 'b)))
    (when clear-between-p (cl-cc/vm::vm-clear-hash-cons-table))
    (let ((c2 (cl-cc/vm::vm-hash-cons 'a 'b)))
      (expect c2 :to-equal c1)
      (if clear-between-p
          (expect (eq c1 c2) :to-be-falsy)
          (expect (eq c1 c2) :to-be-truthy))))))

(it-sequential "vm-hash-cons-instruction-reuses-identical-flat-pairs"
  (let ((s (make-test-vm)))
    (cl-cc/vm::vm-clear-hash-cons-table)
    (cl-cc:vm-reg-set s 1 'a)
    (cl-cc:vm-reg-set s 2 'b)
    (exec1 (cl-cc:make-vm-hash-cons :dst 0 :car-src 1 :cdr-src 2) s)
    (exec1 (cl-cc:make-vm-hash-cons :dst 3 :car-src 1 :cdr-src 2) s)
    (expect (cl-cc:vm-reg-get s 3) :to-be (cl-cc:vm-reg-get s 0))
    (exec1 (cl-cc:make-vm-cons :dst 4 :car-src 1 :cdr-src 2) s)
    (expect (eq (cl-cc:vm-reg-get s 0)
                      (cl-cc:vm-reg-get s 4)) :to-be-falsy)))

(it-sequential "vm-hash-cons-reuses-structurally-equivalent-nested-trees"
  (cl-cc/vm::vm-clear-hash-cons-table)
  (let* ((left-1 (list 'a 'b))
         (right-1 (list 'c 'd))
         (left-2 (list 'a 'b))
         (right-2 (list 'c 'd))
         (t1 (cl-cc/vm::vm-hash-cons left-1 right-1))
         (t2 (cl-cc/vm::vm-hash-cons left-2 right-2)))
    (expect t2 :to-be t1)
    (expect (car t2) :to-be (car t1))
    (expect (cdr t2) :to-be (cdr t1))))

(it-sequential "vm-hash-cons-cyclic-input-does-not-overflow"
  (cl-cc/vm::vm-clear-hash-cons-table)
  (let ((x (cons 'a nil)))
    (setf (cdr x) x)
    (let ((result (cl-cc/vm::vm-hash-cons x 'b)))
      (expect (consp result) :to-be-truthy)
      (expect (cdr result) :to-be 'b))))

(it-sequential "vm-extensible-sequence-builtins"
  (expect (cl-cc/vm::vm-sequence-elt '(a b c) 1) :to-equal 'b)
  (expect (= 3 (cl-cc/vm::vm-sequence-length #(1 2 3))) :to-be-truthy)
  (expect (cl-cc/vm::vm-make-sequence-like '(a) 2 :initial-element 'x) :to-equal '(x x))
  (expect (equalp #(1 2 0 0)
                       (cl-cc/vm::vm-adjust-sequence #(1 2) 4 :initial-element 0)) :to-be-truthy))

(it-sequential "vm-extensible-sequence-specialized-builtins-preserve-type"
  (let ((bits (cl-cc/vm::vm-make-sequence-like #*1 3))
        (more-bits (cl-cc/vm::vm-adjust-sequence #*10 4 :initial-element 1))
        (string (cl-cc/vm::vm-make-sequence-like "x" 3))
        (more-string (cl-cc/vm::vm-adjust-sequence "ab" 4 :initial-element #\c)))
    (expect (typep bits 'bit-vector) :to-be-truthy)
    (expect (equalp #*000 bits) :to-be-truthy)
    (expect (typep more-bits 'bit-vector) :to-be-truthy)
    (expect (equalp #*1011 more-bits) :to-be-truthy)
    (expect (stringp string) :to-be-truthy)
    (expect string :to-equal "   ")
    (expect (stringp more-string) :to-be-truthy)
    (expect more-string :to-equal "abcc")))

(defclass test-sequence ()
  ((payload :initarg :payload :accessor test-sequence-payload)))

(defmethod cl-cc/vm::vm-sequence-elt ((sequence test-sequence) index)
  (aref (test-sequence-payload sequence) index))

(defmethod cl-cc/vm::vm-sequence-length ((sequence test-sequence))
  (length (test-sequence-payload sequence)))

(defmethod cl-cc/vm::vm-make-sequence-like ((sequence test-sequence) size &key (initial-element nil))
  (declare (ignore sequence))
  (make-instance 'test-sequence :payload (make-array size :initial-element initial-element)))

(defclass vm-protocol-backed-sequence (cl-cc/vm::sequence)
  ((payload :initarg :payload :accessor vm-protocol-backed-sequence-payload)))

(defmethod cl-cc/vm::length ((sequence vm-protocol-backed-sequence))
  (length (vm-protocol-backed-sequence-payload sequence)))

(defmethod cl-cc/vm::elt ((sequence vm-protocol-backed-sequence) index)
  (aref (vm-protocol-backed-sequence-payload sequence) index))

(defmethod (setf cl-cc/vm::elt) (value (sequence vm-protocol-backed-sequence) index)
  (setf (aref (vm-protocol-backed-sequence-payload sequence) index) value))

(defmethod cl-cc/vm::make-sequence-like ((sequence vm-protocol-backed-sequence)
                                          size
                                          &key
                                          initial-element)
  (declare (ignore sequence))
  (make-instance 'vm-protocol-backed-sequence
                 :payload (make-array size :initial-element initial-element)))

(it-sequential "vm-extensible-sequence-user-extension"
  (let* ((seq (make-instance 'test-sequence :payload #(10 20 30)))
         (like (cl-cc/vm::vm-make-sequence-like seq 2 :initial-element 7)))
    (expect (= 20 (cl-cc/vm::vm-sequence-elt seq 1)) :to-be-truthy)
    (expect (= 3 (cl-cc/vm::vm-sequence-length seq)) :to-be-truthy)
    (expect (= 2 (cl-cc/vm::vm-sequence-length like)) :to-be-truthy)
    (expect (= 7 (cl-cc/vm::vm-sequence-elt like 0)) :to-be-truthy)))

(it-sequential "vm-extensible-sequence-delegates-to-standard-protocol"
  (let* ((seq (make-instance 'vm-protocol-backed-sequence :payload #(10 20 30)))
         (like (cl-cc/vm::vm-make-sequence-like seq 2 :initial-element 7))
         (grown (cl-cc/vm::vm-adjust-sequence seq 5 :initial-element 9)))
    (expect (= 3 (cl-cc/vm::vm-sequence-length seq)) :to-be-truthy)
    (expect (= 20 (cl-cc/vm::vm-sequence-elt seq 1)) :to-be-truthy)
    (expect (typep like 'vm-protocol-backed-sequence) :to-be-truthy)
    (expect (= 2 (cl-cc/vm::vm-sequence-length like)) :to-be-truthy)
    (expect (= 7 (cl-cc/vm::vm-sequence-elt like 0)) :to-be-truthy)
    (expect (typep grown 'vm-protocol-backed-sequence) :to-be-truthy)
    (expect (= 5 (cl-cc/vm::vm-sequence-length grown)) :to-be-truthy)
    (expect (= 10 (cl-cc/vm::vm-sequence-elt grown 0)) :to-be-truthy)
    (expect (= 30 (cl-cc/vm::vm-sequence-elt grown 2)) :to-be-truthy)
    (expect (= 9 (cl-cc/vm::vm-sequence-elt grown 3)) :to-be-truthy)))

(it-sequential "vm-instructions-use-extensible-sequence-protocol"
  (let ((s (make-test-vm))
        (seq (make-instance 'test-sequence :payload #(10 20 30))))
    (cl-cc:vm-reg-set s 1 seq)
    (cl-cc:vm-reg-set s 2 1)
    (exec1 (cl-cc:make-vm-length :dst 0 :src 1) s)
    (exec1 (cl-cc:make-vm-nth :dst 3 :index 2 :list 1) s)
    (expect (= 3 (cl-cc:vm-reg-get s 0)) :to-be-truthy)
    (expect (= 20 (cl-cc:vm-reg-get s 3)) :to-be-truthy)))

(it-sequential "vm-instructions-use-standard-extensible-sequence-protocol"
  (let ((s (make-test-vm))
        (seq (make-instance 'vm-protocol-backed-sequence :payload #(10 20 30))))
    (cl-cc:vm-reg-set s 1 seq)
    (cl-cc:vm-reg-set s 2 2)
    (exec1 (cl-cc:make-vm-length :dst 0 :src 1) s)
    (exec1 (cl-cc:make-vm-nth :dst 3 :index 2 :list 1) s)
    (expect (= 3 (cl-cc:vm-reg-get s 0)) :to-be-truthy)
    (expect (= 30 (cl-cc:vm-reg-get s 3)) :to-be-truthy)))
