;;;; tests/unit/expand/macros-basic-setf-tests.lisp
;;;; Coverage tests for src/expand/macros-basic.lisp

(in-package :cl-cc/test)



(it-sequential "setf-plain-symbol-to-setq"
  (expect '(setq x v) :to-equal (our-macroexpand-1 '(setf x v))))

(it-sequential "setf-passthrough-expansions gethash"
  (destructuring-bind (form expected-name expected-args) (list '(setf (gethash k ht) v) "SETF-GETHASH" '(k ht v))
    (let ((result (our-macroexpand-1 form)))
    (expect expected-name :to-equal (symbol-name (car result)))
    (expect expected-args :to-equal (cdr result)))))

(it-sequential "setf-passthrough-expansions slot-value"
  (destructuring-bind (form expected-name expected-args) (list '(setf (slot-value obj 'slot) v) "RT-SLOT-SET" '(obj 'slot v))
    (let ((result (our-macroexpand-1 form)))
    (expect expected-name :to-equal (symbol-name (car result)))
    (expect expected-args :to-equal (cdr result)))))

(it-sequential "setf-fill-pointer-expands-to-vm-builtin"
  (let* ((result (our-macroexpand-1 '(setf (fill-pointer v) n)))
         (setter-form (third result)))
    (expect (car result) :to-be 'let)
    (expect (symbol-name (car setter-form)) :to-equal "%SET-FILL-POINTER")
    (expect (second setter-form) :to-be 'v)))

(it-sequential "place-macro-outer-is-let setf-car"
  (destructuring-bind (form) (list '(setf (car x) v))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "place-macro-outer-is-let setf-nth"
  (destructuring-bind (form) (list '(setf (nth 2 lst) v))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "place-macro-outer-is-let setf-getf"
  (destructuring-bind (form) (list '(setf (getf plist :k) v))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "place-macro-outer-is-let case"
  (destructuring-bind (form) (list '(case x (1 :one) (2 :two)))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "place-macro-outer-is-let typecase"
  (destructuring-bind (form) (list '(typecase x (integer :int) (string :str)))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "setf-cons-cell-synonyms car"
  (destructuring-bind (form expected-fn) (list '(setf (car x)   v) 'rplaca)
    (let* ((result (our-macroexpand-1 form))
         (body   (cddr result)))
    (expect expected-fn :to-be (caar body)))))

(it-sequential "setf-cons-cell-synonyms first"
  (destructuring-bind (form expected-fn) (list '(setf (first x) v) 'rplaca)
    (let* ((result (our-macroexpand-1 form))
         (body   (cddr result)))
    (expect expected-fn :to-be (caar body)))))

(it-sequential "setf-cons-cell-synonyms cdr"
  (destructuring-bind (form expected-fn) (list '(setf (cdr x)   v) 'rplacd)
    (let* ((result (our-macroexpand-1 form))
         (body   (cddr result)))
    (expect expected-fn :to-be (caar body)))))

(it-sequential "setf-cons-cell-synonyms rest"
  (destructuring-bind (form expected-fn) (list '(setf (rest x)  v) 'rplacd)
    (let* ((result (our-macroexpand-1 form))
         (body   (cddr result)))
    (expect expected-fn :to-be (caar body)))))

(it-sequential "setf-nth-body-uses-rplaca-nthcdr"
  (let* ((result (our-macroexpand-1 '(setf (nth 2 lst) v)))
         (body   (cddr result)))
    (expect 'rplaca :to-be (caar body))
    (expect 'nthcdr :to-equal (caadar body))))

(it-sequential "setf-getf-body-uses-rt-plist-put"
  (let* ((result    (our-macroexpand-1 '(setf (getf plist :k) v)))
          (setq-form (caddr result)))
    (expect 'setq :to-be (car setq-form))
    (expect "RT-PLIST-PUT" :to-equal (symbol-name (car (caddr setq-form))))))

(it-sequential "setf-getf-compound-place-uses-recursive-setf"
  (let* ((result (our-macroexpand-1 '(setf (getf (cdddr method-entry) :phase) v)))
         (setter-form (caddr result)))
    (expect (car setter-form) :to-be 'setf)
    (expect (second setter-form) :to-equal '(cdddr method-entry))
    (expect "RT-PLIST-PUT" :to-equal (symbol-name (car (third setter-form))))))
