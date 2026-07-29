;;;; tests/unit/expand/macros-control-flow-loop-tests.lisp — Loop/do/case/typecase control-flow tests

(in-package :cl-cc/test)


(it-sequential "dolist-expansion-is-block basic"
  (destructuring-bind (form) (list '(dolist (item list) body))
    (let ((result (our-macroexpand-1 form)))
    (expect 'block :to-be (car result))
    (expect (search "tagbody" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "dolist-expansion-is-block with-result"
  (destructuring-bind (form) (list '(dolist (item list result) body))
    (let ((result (our-macroexpand-1 form)))
    (expect 'block :to-be (car result))
    (expect (search "tagbody" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "dolist-expansion-is-block multi-body"
  (destructuring-bind (form) (list '(dolist (item list) body1 body2 body3))
    (let ((result (our-macroexpand-1 form)))
    (expect 'block :to-be (car result))
    (expect (search "tagbody" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "dotimes-expansion-is-block basic"
  (destructuring-bind (form) (list '(dotimes (i 10) body))
    (let ((result (our-macroexpand-1 form)))
    (expect 'block :to-be (car result))
    (expect (search "tagbody" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "dotimes-expansion-is-block with-result"
  (destructuring-bind (form) (list '(dotimes (i 10 'done) body))
    (let ((result (our-macroexpand-1 form)))
    (expect 'block :to-be (car result))
    (expect (search "tagbody" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "dotimes-expansion-is-block zero-count"
  (destructuring-bind (form) (list '(dotimes (i 0) body))
    (let ((result (our-macroexpand-1 form)))
    (expect 'block :to-be (car result))
    (expect (search "tagbody" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "do-expansion-is-block basic"
  (destructuring-bind (form) (list '(do ((i 0 (1+ i))) ((>= i 10) result) body))
    (let ((result (our-macroexpand-1 form)))
    (expect 'block :to-be (car result))
    (expect (search "tagbody" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "do-expansion-is-block multi-vars"
  (destructuring-bind (form) (list '(do ((i 0 (1+ i)) (j 10 (1- j))) ((= i j) i) body))
    (let ((result (our-macroexpand-1 form)))
    (expect 'block :to-be (car result))
    (expect (search "tagbody" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "do-expansion-is-block no-step"
  (destructuring-bind (form) (list '(do ((x init)) (test result) body))
    (let ((result (our-macroexpand-1 form)))
    (expect 'block :to-be (car result))
    (expect (search "tagbody" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "do-expansion-is-block multi-body"
  (destructuring-bind (form) (list '(do ((i 0)) (test) body1 body2 body3))
    (let ((result (our-macroexpand-1 form)))
    (expect 'block :to-be (car result))
    (expect (search "tagbody" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "do*-expansion-uses-let* basic"
  (destructuring-bind (form) (list '(do* ((i 0 (1+ i)) (j i (1+ j))) ((>= i 10) j) body))
    (let ((result (our-macroexpand-1 form)))
    (expect 'block :to-be (car result))
    (expect (search "let*" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "do*-expansion-uses-let* dep-binding"
  (destructuring-bind (form) (list '(do* ((x 1) (y (+ x 1))) (t y) body))
    (let ((result (our-macroexpand-1 form)))
    (expect 'block :to-be (car result))
    (expect (search "let*" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "case-expansion-is-let basic"
  (destructuring-bind (form) (list '(case key (a body-a) (b body-b)))
    (let ((result (our-macroexpand-1 form)))
    (expect 'let :to-be (car result)))))

(it-sequential "case-expansion-is-let list-of-keys"
  (destructuring-bind (form) (list '(case key ((a b c) body-abc) (d body-d)))
    (let ((result (our-macroexpand-1 form)))
    (expect 'let :to-be (car result)))))

(it-sequential "case-expansion-is-let multi-body"
  (destructuring-bind (form) (list '(case key (a body1 body2 body3)))
    (let ((result (our-macroexpand-1 form)))
    (expect 'let :to-be (car result)))))

(it-sequential "case-default-clause otherwise"
  (destructuring-bind (form) (list '(case key (a body-a) (otherwise default-body)))
    (let ((result (our-macroexpand-1 form)))
    (expect 'let :to-be (car result))
    (expect (search "default-body" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "case-default-clause t-clause"
  (destructuring-bind (form) (list '(case key (a body-a) (t default-body)))
    (let ((result (our-macroexpand-1 form)))
    (expect 'let :to-be (car result))
    (expect (search "default-body" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "case-expands-sparse-integer-keys-into-binary-search"
  (let ((result (our-macroexpand-1
                 '(case key
                    (1 body-1)
                    (8 body-8)
                    (16 body-16)
                    (32 body-32)
                    (otherwise default-body)))))
    (let ((printed (string-downcase (format nil "~S" result))))
      (expect (search "integerp" printed) :to-be-truthy)
      (expect (search "<" printed) :to-be-truthy)
      (expect (search "default-body" printed) :to-be-truthy))))

(it-sequential "case-expands-dense-integer-keys-into-table-dispatch"
  (let ((result (our-macroexpand-1
                 '(case key
                    (1 body-1)
                    (2 body-2)
                    (3 body-3)
                    (4 body-4)
                    (otherwise default-body)))))
    (let ((printed (string-downcase (format nil "~S" result))))
      (expect (search "svref" printed) :to-be-truthy)
      (expect (search "vector" printed) :to-be-truthy)
      (expect (search "br-table" printed) :to-be-truthy)
      (expect (search "(if (integerp" printed) :to-be-truthy)
      (expect (search "default-body" printed) :to-be-truthy))))

(it-sequential "typecase-expansion-is-let basic"
  (destructuring-bind (form) (list '(typecase val (string body-string) (integer body-int)))
    (let ((result (our-macroexpand-1 form)))
    (expect 'let :to-be (car result)))))

(it-sequential "typecase-expansion-is-let multi-body"
  (destructuring-bind (form) (list '(typecase val (string body1 body2)))
    (let ((result (our-macroexpand-1 form)))
    (expect 'let :to-be (car result)))))

(it-sequential "typecase-default-clause otherwise"
  (destructuring-bind (form) (list '(typecase val (string body-string) (otherwise default-body)))
    (let ((result (our-macroexpand-1 form)))
    (expect 'let :to-be (car result))
    (expect (search "default-body" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "typecase-default-clause t-clause"
  (destructuring-bind (form) (list '(typecase val (string body-string) (t default-body)))
    (let ((result (our-macroexpand-1 form)))
    (expect 'let :to-be (car result))
    (expect (search "default-body" (string-downcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "typecase-prunes-subsumed-later-clause"
  (let* ((result (our-macroexpand-1 '(typecase v
                                      (number body-number)
                                      (fixnum body-fixnum)
                                      (otherwise default-body))))
         (printed (string-downcase (format nil "~S" result))))
    (expect (search "body-number" printed) :to-be-truthy)
    (expect (search "body-fixnum" printed) :to-be-falsy)
    (expect (search "default-body" printed) :to-be-truthy)))

(it-sequential "typecase-expands-disjoint-arms-into-decision-tree"
  (let* ((result (our-macroexpand-1
                  '(typecase v
                     (string body-string)
                     (symbol body-symbol)
                     (integer body-int)
                     (otherwise body-default)))))
    (expect result :to-be-truthy)
    (expect (consp result) :to-be-truthy)))

(it-sequential "typecase-overlapping-arms-use-ordered-decision-tree"
  (let* ((result (our-macroexpand-1
                  '(typecase v
                      (number body-number)
                      (integer body-int)
                      (string body-string)
                      (symbol body-symbol)
                      (otherwise body-default))))
         (printed (string-downcase (format nil "~S" result))))
    (expect (search "(or (typep" printed) :to-be-truthy)
    (expect (search "body-number" printed) :to-be-truthy)
    (expect (search "body-default" printed) :to-be-truthy)))

;;; ─── Direct helper unit tests ──────────────────────────────────────────────

(it-sequential "case-build-eql-chain-empty"
  (expect (cl-cc/expand::%case-build-eql-chain '() 'key 'default-body) :to-equal 'default-body))

(it-sequential "case-build-eql-chain-single-key atom-key"
  (destructuring-bind (cases expected) (list '((a body-a)) '(if (eql key 'a) (progn body-a) nil))
    (expect (cl-cc/expand::%case-build-eql-chain cases 'key nil) :to-equal expected)))

(it-sequential "case-build-eql-chain-single-key list-keys"
  (destructuring-bind (cases expected) (list '(((a b) body-ab)) '(if (or (eql key 'a) (eql key 'b)) (progn body-ab) nil))
    (expect (cl-cc/expand::%case-build-eql-chain cases 'key nil) :to-equal expected)))

(it-sequential "case-build-eql-chain-otherwise-terminates"
  (let ((result (cl-cc/expand::%case-build-eql-chain
                 '((otherwise fallback)) 'key 'default)))
    (expect result :to-equal '(progn fallback))))

(it-sequential "case-collect-integer-pairs-extracts-default"
  (multiple-value-bind (default pairs integer-only-p)
      (cl-cc/expand::%case-collect-integer-pairs
       '((1 body-1) (2 body-2) (otherwise fallback)))
    (expect default :to-equal '(progn fallback))
    (expect (= 2 (length pairs)) :to-be-truthy)
    (expect integer-only-p :to-be-truthy)))

(it-sequential "case-collect-integer-pairs-non-integer-flag"
  (multiple-value-bind (default pairs integer-only-p)
      (cl-cc/expand::%case-collect-integer-pairs
       '((1 body-1) (foo body-foo)))
    (declare (ignore default pairs))
    (expect integer-only-p :to-be-falsy)))

(it-sequential "typecase-build-typep-chain-empty"
  (expect (cl-cc/expand::%typecase-build-typep-chain '() 'val) :to-be-null))

(it-sequential "typecase-build-typep-chain-single"
  (expect (cl-cc/expand::%typecase-build-typep-chain
                 '((string body-s)) 'val) :to-equal '(if (typep val 'string) (progn body-s) nil)))

(it-sequential "typecase-build-typep-chain-otherwise"
  (expect (cl-cc/expand::%typecase-build-typep-chain
                 '((otherwise fallback)) 'val) :to-equal '(progn fallback)))

(it-sequential "typecase-related-types-p-detects-subtype-links"
  (expect (cl-cc/expand::%typecase-related-types-p 'integer 'number) :to-be-truthy)
  (expect (cl-cc/expand::%typecase-related-types-p 'number 'integer) :to-be-truthy)
  (expect (cl-cc/expand::%typecase-related-types-p 'string 'symbol) :to-be-falsy))

(it-sequential "typecase-choose-split-index-prefers-low-cross-overlap"
  (let* ((cases '((integer body-int)
                  (number body-num)
                  (string body-str)
                  (symbol body-sym)))
         (idx (cl-cc/expand::%typecase-choose-split-index cases)))
    (expect (= 2 idx) :to-be-truthy)))
