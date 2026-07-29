;;;; tests/unit/expand/expander-setf-places-tests.lisp — Setf-place expander tests

(in-package :cl-cc/test)



(it-sequential "expander-setf-slot-value-passthrough"
  (let ((result (cl-cc/expand:compiler-macroexpand-all
                  '(setf (slot-value obj 'field) new-val))))
    (expect (first result) :to-be 'setf)
    (assert-form-string-not-contains result "SETF-SLOT-VALUE")))

(it-sequential "expand-setf-accessor-unknown-falls-back-to-slot-value"
  (let ((result (cl-cc/expand::expand-setf-accessor '(some-unknown-accessor obj) 'val)))
    (assert-form-string-contains result "RT-SLOT-SET")))

(it-sequential "expand-setf-accessor-known-maps-to-slot-name"
  (setf (gethash 'test-reg-accessor cl-cc/expand:*accessor-slot-map*)
        (cons 'test-class 'the-slot))
  (let ((result (cl-cc/expand::expand-setf-accessor '(test-reg-accessor obj) 'new-val)))
    (assert-form-string-contains result "THE-SLOT")))

(it-sequential "expand-setf-cons-place car"
  (destructuring-bind (place expected-fn) (list '(car x) "RPLACA")
    (let ((result (cl-cc/expand::expand-setf-cons-place place 'val)))
    (expect (first result) :to-be 'let)
    (assert-form-string-contains result expected-fn))))

(it-sequential "expand-setf-cons-place cdr"
  (destructuring-bind (place expected-fn) (list '(cdr x) "RPLACD")
    (let ((result (cl-cc/expand::expand-setf-cons-place place 'val)))
    (expect (first result) :to-be 'let)
    (assert-form-string-contains result expected-fn))))

(it-sequential "expander-setf-accessor-slot-value-fallback"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(setf (my-unknown-accessor-xyz obj) v))))
    (assert-form-string-contains result "RT-SLOT-SET")))

(it-sequential "expander-setf-nth-place"
  (assert-expanded-string-contains '(setf (nth 2 lst) newval) "RPLACA")
  (assert-expanded-string-contains '(setf (nth 2 lst) newval) "NTHCDR"))

(it-sequential "expander-setf-list-accessor-places second"
  (destructuring-bind (form expected-op expected-traversal) (list '(setf (second x) newval) "RPLACA" "CDR")
    (assert-expanded-string-contains form expected-op) (assert-expanded-string-contains form expected-traversal) (assert-expanded-string-not-contains form "RT-SLOT-SET")))

(it-sequential "expander-setf-list-accessor-places tenth"
  (destructuring-bind (form expected-op expected-traversal) (list '(setf (tenth x) newval) "RPLACA" "CDR")
    (assert-expanded-string-contains form expected-op) (assert-expanded-string-contains form expected-traversal) (assert-expanded-string-not-contains form "RT-SLOT-SET")))

(it-sequential "expander-setf-cxr-compound-places cadr"
  (destructuring-bind (form expected-op) (list '(setf (cadr x) newval) "RPLACA")
    (let ((*print-circle* t))
    (assert-expanded-string-contains form expected-op)
    (assert-expanded-string-contains form "CDR"))))

(it-sequential "expander-setf-cxr-compound-places cddr"
  (destructuring-bind (form expected-op) (list '(setf (cddr x) newval) "RPLACD")
    (let ((*print-circle* t))
    (assert-expanded-string-contains form expected-op)
    (assert-expanded-string-contains form "CDR"))))

(it-sequential "expander-setf-getf-place"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(setf (getf my-plist :foo) 42))))
    (expect (car result) :to-be 'let)
    (assert-form-string-contains result "RT-PLIST-PUT")))

(it-sequential "expander-setf-getf-compound-cxr-place"
  (let ((result (cl-cc/expand:compiler-macroexpand-all
                  '(setf (getf (cdddr method-entry) :phase) :primary))))
    (expect (car result) :to-be 'let)
    (assert-form-string-contains result "RT-PLIST-PUT")
    (assert-form-string-contains result "RPLACD")
    (assert-form-string-not-contains result "SETQ (CDDDR")))

(it-sequential "expander-setf-fill-pointer-place"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(setf (fill-pointer vec) 3))))
    (expect (symbol-name (car result)) :to-equal "%SET-FILL-POINTER")
    (expect (second result) :to-be 'vec)
    (expect (= 3 (third result)) :to-be-truthy)))

(it-sequential "expander-setf-get-place"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(setf (get my-sym :foo) 42))))
    (expect (car result) :to-be 'let)
    (assert-form-string-contains result "%SET-SYMBOL-PLIST")
    (assert-form-string-contains result "SYMBOL-PLIST")
    (assert-form-string-contains result "RT-PLIST-PUT")))

(it-sequential "expander-setf-symbol-value-place"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(setf (symbol-value my-sym) 42))))
    (expect (car result) :to-be 'let)
    (assert-form-string-contains result "*RUNTIME-SET-SYMBOL-VALUE-FN*")
    (assert-form-string-contains result "MY-SYM")))

(it-sequential "expander-setf-simple-runtime-bridges find-class"
  (destructuring-bind (form expected-op) (list '(setf (find-class sample-class) klass) "%SET-FIND-CLASS")
    (assert-expanded-string-contains form expected-op)))

(it-sequential "expander-setf-simple-runtime-bridges macro-function"
  (destructuring-bind (form expected-op) (list '(setf (macro-function sample-macro) new-fn) "%SET-MACRO-FUNCTION")
    (assert-expanded-string-contains form expected-op)))

(it-sequential "expander-setf-simple-runtime-bridges symbol-function"
  (destructuring-bind (form expected-op) (list '(setf (symbol-function sample-fn) new-fn) "SET-FDEFINITION")
    (assert-expanded-string-contains form expected-op)))

(it-sequential "expander-setf-simple-runtime-bridges fdefinition"
  (destructuring-bind (form expected-op) (list '(setf (fdefinition sample-fn) new-fn) "SET-FDEFINITION")
    (assert-expanded-string-contains form expected-op)))

(it-sequential "expander-setf-simple-runtime-bridges svref"
  (destructuring-bind (form expected-op) (list '(setf (svref vec 1) 42) "%SVSET")
    (assert-expanded-string-contains form expected-op)))

(it-sequential "expander-setf-simple-runtime-bridges row-major-aref"
  (destructuring-bind (form expected-op) (list '(setf (row-major-aref arr 2) 42) "ASET")
    (assert-expanded-string-contains form expected-op)))

(it-sequential "expander-setf-shared-string-and-bit-places char"
  (destructuring-bind (form expected-op) (list '(setf (char s 0) #\A) "RT-STRING-SET")
    (assert-expanded-string-contains form expected-op)))

(it-sequential "expander-setf-shared-string-and-bit-places schar"
  (destructuring-bind (form expected-op) (list '(setf (schar s 1) #\B) "RT-STRING-SET")
    (assert-expanded-string-contains form expected-op)))

(it-sequential "expander-setf-shared-string-and-bit-places bit"
  (destructuring-bind (form expected-op) (list '(setf (bit bv 2) 1) "RT-BIT-SET")
    (assert-expanded-string-contains form expected-op)))

(it-sequential "expander-setf-shared-string-and-bit-places sbit"
  (destructuring-bind (form expected-op) (list '(setf (sbit bv 3) 0) "RT-BIT-SET")
    (assert-expanded-string-contains form expected-op)))

(it-sequential "expander-setf-subseq-place"
  (let ((result (cl-cc/expand:compiler-macroexpand-all
                  '(setf (subseq seq 1 4) replacement))))
    (assert-form-string-contains result "ASET")
    (assert-form-string-contains result "SETF")
    (assert-form-string-contains result "ELT")
    (assert-form-string-contains result "4")))

(it-sequential "expander-setf-subseq-place-without-end"
  (let ((result (cl-cc/expand:compiler-macroexpand-all
                  '(setf (subseq seq 1) replacement))))
    (assert-form-string-contains result "ASET")
    (assert-form-string-contains result "SETF")
    (assert-form-string-contains result "ELT")
    (assert-form-string-contains result "LENGTH")))

(it-sequential "expander-setf-values-place"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(setf (values a b) (floor 5 2)))))
    (expect (car result) :to-be 'let)
    (assert-form-string-contains result "%VALUES-TO-LIST")
    (expect (car (third result)) :to-be 'setq)
    (expect (second (third result)) :to-be 'a)
    (expect (car (fourth result)) :to-be 'setq)
    (expect (second (fourth result)) :to-be 'b)
    (expect (car (fifth result)) :to-be 'car)))
