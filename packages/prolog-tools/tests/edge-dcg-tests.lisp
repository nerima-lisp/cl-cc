;;;; packages/prolog-tools/tests/edge-dcg-tests.lisp

(in-package :cl-cc/prolog-tools-tests)

(describe "edge-spec-well-formed-p"
  (it "accepts a single edge"
    (expect (edge-spec-well-formed-p (tokenize-edge-spec "main -> helper")) :to-be-truthy))

  (it "accepts a comma-separated edge list"
    (expect (edge-spec-well-formed-p (tokenize-edge-spec "main -> helper, helper -> leaf"))
            :to-be-truthy))

  (it "rejects an empty spec"
    (expect (edge-spec-well-formed-p (tokenize-edge-spec "")) :to-be-falsy))

  (it "rejects a spec with a missing right-hand side"
    (expect (edge-spec-well-formed-p (tokenize-edge-spec "main ->")) :to-be-falsy))

  (it "rejects a spec with a stray trailing comma"
    (expect (edge-spec-well-formed-p (tokenize-edge-spec "main -> helper,")) :to-be-falsy)))

(describe "parse-edge-spec"
  (it "parses a single edge into a (caller . callee) pair"
    (expect (parse-edge-spec "main -> helper") :to-equal '((:main . :helper))))

  (it "parses a comma-separated edge list"
    (expect (parse-edge-spec "main -> helper, helper -> leaf")
            :to-equal '((:main . :helper) (:helper . :leaf))))

  (it "signals an error on malformed input"
    (expect (lambda () (parse-edge-spec "main -> -> leaf")) :to-throw))

  (it-property "every parsed pair's caller and callee were tokens in the source"
      ((names (gen-list (gen-member '("a" "b" "c" "d")) :min-length 1 :max-length 5)))
    (let ((spec (format nil "~{~A~^, ~}"
                         (loop for (from to) on names by #'cddr
                               while to
                               collect (format nil "~A -> ~A" from to)))))
      (when (plusp (length spec))
        (let ((pairs (parse-edge-spec spec)))
          (dolist (pair pairs)
            (expect (keywordp (car pair)) :to-be-truthy)
            (expect (keywordp (cdr pair)) :to-be-truthy)))))))
