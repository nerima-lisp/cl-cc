;;;; packages/prolog-tools/src/edge-dcg.lisp - DCG grammar for a textual edge notation
;;;;
;;;; A small notation for describing call edges outside of Lisp source,
;;;; e.g. "main -> helper, helper -> leaf", useful for fixtures and for
;;;; hand-authored call-graph descriptions. The grammar itself is defined
;;;; with CL-PROLOG:DEF-DCG-RULE and used purely as a *recognizer*
;;;; (EDGE-SPEC-WELL-FORMED-P) via CL-PROLOG:PHRASE-ALL: CL-PROLOG:DCG-STAR
;;;; emits its shortest (zero-repetition) match first, so the caller must
;;;; scan every returned remainder for NIL (full consumption) rather than
;;;; trust the first solution. Structured extraction of the parsed pairs is
;;;; done separately by PARSE-EDGE-SPEC, an ordinary (non-backtracking)
;;;; Lisp pass over the same token stream — DCG body goals only see the
;;;; stream, not an accumulator, and a side-effecting accumulator would
;;;; double-count values across the grammar's own backtracking attempts.

(in-package :cl-cc/prolog-tools)

(defun tokenize-edge-spec (spec)
  "Tokenize a string like \"main -> helper, helper -> leaf\" into a list of
tokens: (:ID . NAME), :ARROW, and :COMMA."
  (let ((tokens '())
        (length (length spec))
        (position 0))
    (flet ((skip-whitespace ()
             (loop while (and (< position length) (member (char spec position) '(#\Space #\Tab #\Newline)))
                   do (incf position))))
      (loop
        (skip-whitespace)
        (when (>= position length) (return))
        (let ((ch (char spec position)))
          (cond
            ((char= ch #\,) (push :comma tokens) (incf position))
            ((and (char= ch #\-) (< (1+ position) length) (char= (char spec (1+ position)) #\>))
             (push :arrow tokens) (incf position 2))
            ((or (alpha-char-p ch) (char= ch #\_))
             (let ((start position))
               (loop while (and (< position length)
                                 (or (alphanumericp (char spec position)) (char= (char spec position) #\_)))
                     do (incf position))
               (push (cons :id (intern (string-upcase (subseq spec start position)) :keyword)) tokens)))
            (t (error "Unexpected character ~C in edge spec at position ~D" ch position))))))
    (nreverse tokens)))

(defvar *edge-grammar-rulebase*
  (cl-prolog:rulebase-extend
   (cl-prolog:make-rulebase)
   (list (cl-prolog:def-dcg-rule edge
           (cl-prolog:dcg-token-match-value :id ?caller)
           (terminal :arrow)
           (cl-prolog:dcg-token-match-value :id ?callee))
         (cl-prolog:def-dcg-rule edge-tail
           (terminal :comma)
           edge)
         (cl-prolog:def-dcg-rule edge-list
           edge
           (cl-prolog:dcg-star edge-tail))))
  "Grammar: EDGE-LIST --> EDGE (',' EDGE)*, where EDGE --> ID '->' ID.")

(defun edge-spec-well-formed-p (tokens)
  "True when TOKENS is a complete parse of the EDGE-LIST grammar."
  (let ((remainders (cl-prolog:phrase-all *edge-grammar-rulebase* 'edge-list tokens)))
    (not (null (member nil remainders :test #'equal)))))

(defun parse-edge-spec (spec)
  "Tokenize and parse SPEC into a list of (CALLER . CALLEE) symbol pairs.

Signals an error if SPEC is not well-formed per the EDGE-LIST grammar."
  (let ((tokens (tokenize-edge-spec spec)))
    (unless (edge-spec-well-formed-p tokens)
      (error "Malformed edge spec: ~S" spec))
    (loop while tokens
          collect (let* ((caller (cdr (pop tokens)))
                         (arrow (pop tokens))
                         (callee (cdr (pop tokens))))
                    (declare (ignore arrow))
                    (when (eq (first tokens) :comma) (pop tokens))
                    (cons caller callee)))))
