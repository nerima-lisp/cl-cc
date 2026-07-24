(in-package :cl-cc/test)


(defun %php-first (src)
  "Parse SRC and return the first top-level AST node."
  (first (cl-cc/php:parse-php-source src)))

(defun %php-first-binding-value (src)
  "Parse SRC as an assignment and return the value expression from the first binding."
  (let ((ast (%php-first src)))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy)
    (cdr (first (cl-cc:ast-let-bindings ast)))))

(defun %php-call-name (ast)
  "Return the symbol name for an AST-CALL function when it is a variable."
  (when (and (cl-cc:ast-call-p ast)
             (cl-cc:ast-var-p (cl-cc:ast-call-func ast)))
    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func ast)))))

(defun %php-assert-full-source-unsupported (src)
  "Assert that checking every parsed form in SRC rejects unsupported PHP."
  (signals error (cl-cc/php:php-check-supported-forms
     (cl-cc/php:parse-php-source src))))
