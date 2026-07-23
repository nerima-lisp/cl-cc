;;;; tests/unit/expand/loop-parser-tests.lisp — LOOP parser layer tests

(in-package :cl-cc/test)



(it-sequential "loop-kw-p-is-case-insensitive"
  (expect (cl-cc/expand::loop-kw-p 'for "FOR") :to-be-truthy)
  (expect (cl-cc/expand::loop-kw-p 'FoR "for") :to-be-truthy)
  (expect (cl-cc/expand::loop-kw-p 'collect "FOR") :to-be-falsy))

(it-sequential "loop-kw-member-p-recognizes-boundaries"
  (expect (cl-cc/expand::loop-kw-member-p 'collect) :to-be-truthy)
  (expect (cl-cc/expand::loop-kw-member-p 'finally) :to-be-truthy)
  (expect (cl-cc/expand::loop-kw-member-p 'foo) :to-be-falsy))

(it-sequential "loop-finalize-state-normalizes-reversed-lists"
  (let ((state (cl-cc/expand::make-loop-parse-state)))
    (setf (cl-cc/expand::lps-iterations state) '(b a)
          (cl-cc/expand::lps-body-forms state) '((body-2) (body-1))
          (cl-cc/expand::lps-accumulations state) '((:collect 2 nil nil) (:collect 1 nil nil))
          (cl-cc/expand::lps-conditions state) '((:while b) (:while a))
          (cl-cc/expand::lps-initially state) '((init-2) (init-1))
          (cl-cc/expand::lps-finally state) '((fin-2) (fin-1))
          (cl-cc/expand::lps-loop-name state) 'demo)
    (let ((result (cl-cc/expand::finalize-loop-state state)))
      (expect (getf result :iterations) :to-equal '(a b))
      (expect (getf result :body) :to-equal '((body-2) (body-1)))
      (expect (getf result :accumulations) :to-equal '((:collect 1 nil nil) (:collect 2 nil nil)))
      (expect (getf result :conditions) :to-equal '((:while a) (:while b)))
      (expect (getf result :initially) :to-equal '((init-1) (init-2)))
      (expect (getf result :finally) :to-equal '((fin-1) (fin-2)))
      (expect (getf result :loop-name) :to-be 'demo))))

(it-sequential "loop-parse-clauses-dispatches-core-clauses"
  (let ((result (cl-cc/expand::parse-loop-clauses
                 '(named demo
                   do (print :hello)
                   collect x into xs
                   finally (list :done)))))
    (expect (getf result :loop-name) :to-be 'demo)
    (expect (getf result :accumulations) :to-equal '((:collect x xs nil)))
    (expect (getf result :body) :to-equal '((print :hello)))
    (expect (getf result :finally) :to-equal '((list :done)))))
