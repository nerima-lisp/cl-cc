(in-package :cl-cc/test)

(it-sequential "php-parser-switch-case-default-statement"
  (let ((ast (%php-first "<?php switch ($x) { case 1: echo 'one'; break; default: echo 'other'; }")))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy)
    (let ((block (first (cl-cc:ast-let-body ast))))
      (expect (cl-cc:ast-block-p block) :to-be-truthy)
      (let ((tagbody (first (cl-cc:ast-block-body block))))
        (expect (cl-cc:ast-tagbody-p tagbody) :to-be-truthy)
        (expect (some (lambda (section)
                             (some #'cl-cc:ast-if-p (cdr section)))
                           (cl-cc:ast-tagbody-tags tagbody)) :to-be-truthy)
        (expect (some (lambda (section)
                             (some #'cl-cc:ast-go-p (cdr section)))
                           (cl-cc:ast-tagbody-tags tagbody)) :to-be-truthy)))))

(it-sequential "php-parser-break-continue-level-statements"
  (let ((ast (%php-first "<?php while ($a) { while ($b) { continue 2; break 2; } }")))
    (expect (cl-cc:ast-block-p ast) :to-be-truthy)
    (let ((outer-tagbody (first (cl-cc:ast-block-body ast))))
      (expect (cl-cc:ast-tagbody-p outer-tagbody) :to-be-truthy)
      (expect (some (lambda (section)
                           (some (lambda (form)
                                   (and (cl-cc:ast-block-p form)
                                        (cl-cc:ast-tagbody-p (first (cl-cc:ast-block-body form)))))
                                 (cdr section)))
                         (cl-cc:ast-tagbody-tags outer-tagbody)) :to-be-truthy))))

(it-sequential "php-parser-try-catch-finally-statement"
  (let ((ast (%php-first "<?php try { throw new Ex(); } catch (Ex $e) { echo $e; } finally { echo 'done'; }")))
    (expect (cl-cc:ast-unwind-protect-p ast) :to-be-truthy)
    (let ((inner (cl-cc:ast-unwind-protected ast)))
      (expect (cl-cc:ast-let-p inner) :to-be-truthy)
      (let ((dispatch (first (cl-cc:ast-let-body inner))))
        (expect (cl-cc:ast-if-p dispatch) :to-be-truthy)))))

(it-sequential "php-parser-catch-union-types"
  (let* ((ast (%php-first "<?php try { throw new ExA(); } catch (ExA | ExB $e) { echo $e; }"))
         (inner (cl-cc:ast-unwind-protected ast))
         (top-dispatch (first (cl-cc:ast-let-body inner)))
         (catch-dispatch (cl-cc:ast-if-then top-dispatch))
         (match-cond (cl-cc:ast-if-cond catch-dispatch))
         (class-arg (second (cl-cc:ast-call-args match-cond))))
    (expect (cl-cc:ast-call-p match-cond) :to-be-truthy)
    (expect (mapcar #'symbol-name (cl-cc:ast-quote-value class-arg)) :to-equal '("EXA" "EXB"))))

(it-sequential "php-parser-throw-statement"
  (let ((ast (%php-first "<?php throw new Ex();")))
    (expect (cl-cc:ast-throw-p ast) :to-be-truthy)
    (let ((tag-val (cl-cc:ast-quote-value (cl-cc:ast-throw-tag ast))))
      (expect (symbolp tag-val) :to-be-truthy)
      (expect (search "EXCEPTION" (symbol-name tag-val)) :to-be-truthy))
    (expect (cl-cc:ast-call-p (cl-cc:ast-throw-value ast)) :to-be-truthy)
    (expect (%php-call-name (cl-cc:ast-throw-value ast)) :to-equal "%PHP-MAKE-EXCEPTION")))

(it-sequential "php-parser-short-array-literal"
  (let ((value (%php-first-binding-value "<?php $xs = [1, 2, 3];")))
    (expect (cl-cc:ast-call-p value) :to-be-truthy)
    (expect (%php-call-name value) :to-equal "%PHP-ARRAY")
    (expect (= 3 (length (cl-cc:ast-call-args value))) :to-be-truthy)
    (expect (every #'cl-cc:ast-list-p (cl-cc:ast-call-args value)) :to-be-truthy)))

(it-sequential "php-parser-associative-array-literal"
  (let ((value (%php-first-binding-value "<?php $map = [\"a\" => 1, \"b\" => 2];")))
    (expect (cl-cc:ast-call-p value) :to-be-truthy)
    (expect (%php-call-name value) :to-equal "%PHP-ARRAY")
    (expect (= 2 (length (cl-cc:ast-call-args value))) :to-be-truthy)
    (expect (every (lambda (entry)
              (and (cl-cc:ast-list-p entry)
                   (cl-cc:ast-quote-value (first (cl-cc:ast-list-elements entry)))))
            (cl-cc:ast-call-args value)) :to-be-truthy)))

(it-sequential "php-parser-array-function-style-literal"
  (let ((value (%php-first-binding-value "<?php $xs = array(1, 2, 3);")))
    (expect (cl-cc:ast-call-p value) :to-be-truthy)
    (expect (%php-call-name value) :to-equal "%PHP-ARRAY")
    (expect (= 3 (length (cl-cc:ast-call-args value))) :to-be-truthy)))

(it-sequential "php-parser-array-element-access"
  (let ((value (%php-first-binding-value "<?php $x = $a[0];")))
    (expect (cl-cc:ast-call-p value) :to-be-truthy)
    (expect (%php-call-name value) :to-equal "%PHP-ARRAY-REF")
    (expect (= 2 (length (cl-cc:ast-call-args value))) :to-be-truthy)))

(it-sequential "php-parser-array-element-assignment"
  (let ((ast (%php-first "<?php $a[0] = $v;")))
    (expect (cl-cc:ast-call-p ast) :to-be-truthy)
    (expect (%php-call-name ast) :to-equal "%PHP-ARRAY-SET")
    (expect (= 3 (length (cl-cc:ast-call-args ast))) :to-be-truthy)))

(it-sequential "php-parser-compound-assignment-variable"
  (let* ((asts (cl-cc/php:parse-php-source "<?php $x = 0; $x += 5;"))
         ;; $x=0 let wraps $x+=5 in its body; $x+=5 is first in that body
         (compound (first (cl-cc:ast-let-body (first asts)))))
    (expect (cl-cc:ast-let-p compound) :to-be-truthy)
    (let ((setq (first (cl-cc:ast-let-body compound))))
      (expect (cl-cc:ast-setq-p setq) :to-be-truthy)
      (expect (symbol-name (cl-cc:ast-setq-var setq)) :to-equal "x")
      (let ((value (cl-cc:ast-setq-value setq)))
        (expect (cl-cc:ast-call-p value) :to-be-truthy)
        (expect (%php-call-name value) :to-equal "%PHP-ADD")))))

(it-sequential "php-parser-compound-assignment-array-element"
  (let ((ast (%php-first "<?php $arr[0] += 1;")))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy)
    (let ((set-call (first (cl-cc:ast-let-body ast))))
      (expect (cl-cc:ast-call-p set-call) :to-be-truthy)
      (expect (%php-call-name set-call) :to-equal "%PHP-ARRAY-SET")
      (let ((value (third (cl-cc:ast-call-args set-call))))
        (expect (cl-cc:ast-call-p value) :to-be-truthy)
        (expect (%php-call-name value) :to-equal "%PHP-ADD")
        (expect (%php-call-name (first (cl-cc:ast-call-args value))) :to-equal "%PHP-ARRAY-REF")))))

(it-sequential "php-parser-null-coalescing-assignment-variable"
  (let* ((outer (%php-first "<?php $x = 1; $x ??= 42;"))
         (ast (first (cl-cc:ast-let-body outer))))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy)
    (let ((if-node (first (cl-cc:ast-let-body ast))))
      (expect (cl-cc:ast-if-p if-node) :to-be-truthy)
      (expect (cl-cc:ast-setq-p (cl-cc:ast-if-then if-node)) :to-be-truthy)
      (expect (symbol-name (cl-cc:ast-setq-var (cl-cc:ast-if-then if-node))) :to-equal "x"))))

(it-sequential "php-parser-compound-assignment-property"
  (let ((ast (%php-first "<?php $obj->count *= 3;")))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy)
    (let ((slot-set (first (cl-cc:ast-let-body ast))))
      (expect (cl-cc:ast-set-slot-value-p slot-set) :to-be-truthy)
      (expect (symbol-name (cl-cc:ast-set-slot-value-slot slot-set)) :to-equal "COUNT")
      (let ((value (cl-cc:ast-set-slot-value-value slot-set)))
        (expect (cl-cc:ast-call-p value) :to-be-truthy)
        (expect (%php-call-name value) :to-equal "%PHP-MUL")))))

(it-sequential "php-parser-all-compound-assignment-operators-parse"
  (dolist (op '("+=" "-=" "*=" "/=" ".=" "%=" "**=" "&=" "|=" "^=" "<<=" ">>=" "??="))
    ;; `$x = 1' lowers to (let ((x 1)) BODY); the compound form is BODY[0].
    (let* ((outer (%php-first (format nil "<?php $x = 1; $x ~A 2;" op)))
           (ast (first (cl-cc:ast-let-body outer))))
      (expect (cl-cc:ast-let-p ast) :to-be-truthy)
      (expect (first (cl-cc:ast-let-body ast)) :to-be-truthy))))

(it-sequential "php-parser-unset-array-element-lowering"
  (let ((ast (%php-first "<?php unset($a[0]);")))
    (expect (cl-cc:ast-call-p ast) :to-be-truthy)
    (expect (%php-call-name ast) :to-equal "%PHP-ARRAY-UNSET")
    (expect (= 2 (length (cl-cc:ast-call-args ast))) :to-be-truthy)))

(it-sequential "php-parser-unset-object-property-lowering"
  (let ((ast (%php-first "<?php unset($o->x);")))
    (expect (cl-cc:ast-set-slot-value-p ast) :to-be-truthy)
    (expect (symbol-name (cl-cc:ast-set-slot-value-slot ast)) :to-equal "X")
    (expect (cl-cc:ast-quote-p (cl-cc:ast-set-slot-value-value ast)) :to-be-truthy)))

(it-sequential "php-parser-declare-block-keeps-body"
  (let ((ast (%php-first "<?php declare(ticks=1) { echo 'a'; echo 'b'; }")))
    (expect (cl-cc:ast-progn-p ast) :to-be-truthy)
    (expect (= 2 (length (cl-cc:ast-progn-forms ast))) :to-be-truthy)
    (expect (every #'cl-cc:ast-call-p (cl-cc:ast-progn-forms ast)) :to-be-truthy)))

(it-sequential "php-parser-declare-alternative-keeps-body"
  (let ((ast (%php-first "<?php declare(ticks=1): echo 'a'; echo 'b'; enddeclare;")))
    (expect (cl-cc:ast-progn-p ast) :to-be-truthy)
    (expect (= 2 (length (cl-cc:ast-progn-forms ast))) :to-be-truthy)
    (expect (every #'cl-cc:ast-call-p (cl-cc:ast-progn-forms ast)) :to-be-truthy)))

(it-sequential "php-parser-close-tag-is-accepted"
  (let ((asts (cl-cc/php:parse-php-source "<?php echo 1; ?>")))
    (expect (= 1 (length asts)) :to-be-truthy)
    (expect (cl-cc:ast-call-p (first asts)) :to-be-truthy)
    (expect (%php-call-name (first asts)) :to-equal "%PHP-OUTPUT-WRITE")))

(it-sequential "php-parser-inline-html-between-tags"
  (let ((asts (cl-cc/php:parse-php-source "<?php echo 1; ?>hello<?php echo 2;")))
    (expect (= 3 (length asts)) :to-be-truthy)
    (expect (every (lambda (a)
                          (and (cl-cc:ast-call-p a)
                               (string= "%PHP-OUTPUT-WRITE" (%php-call-name a))))
                        asts) :to-be-truthy)))

(it-sequential "php-parser-array-spread-syntax spread-only"
  (destructuring-bind (src) (list "<?php $a = [...$b, ...$c];")
    (expect (%php-first src) :to-be-truthy)))

(it-sequential "php-parser-array-spread-syntax spread-mixed"
  (destructuring-bind (src) (list "<?php $a = [1, ...$b, 2];")
    (expect (%php-first src) :to-be-truthy)))

(it-sequential "php-parser-list-destructuring two-targets"
  (destructuring-bind (src expected-bindings) (list "<?php [$a, $b] = $arr;" 2)
    (let ((ast (%php-first src)))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy)
    (expect (= expected-bindings (length (cl-cc:ast-let-bindings ast))) :to-be-truthy))))

(it-sequential "php-parser-list-destructuring three-targets"
  (destructuring-bind (src expected-bindings) (list "<?php [$x, $y, $z] = $data;" 3)
    (let ((ast (%php-first src)))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy)
    (expect (= expected-bindings (length (cl-cc:ast-let-bindings ast))) :to-be-truthy))))


(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
