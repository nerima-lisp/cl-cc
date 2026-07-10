(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest php-parser-switch-case-default-statement
  "switch/case/default lowers to let + block + tagbody dispatch/fallthrough control flow."
  (let ((ast (%php-first "<?php switch ($x) { case 1: echo 'one'; break; default: echo 'other'; }")))
    (assert-true (cl-cc:ast-let-p ast))
    (let ((block (first (cl-cc:ast-let-body ast))))
      (assert-true (cl-cc:ast-block-p block))
      (let ((tagbody (first (cl-cc:ast-block-body block))))
        (assert-true (cl-cc:ast-tagbody-p tagbody))
        (assert-true (some (lambda (section)
                             (some #'cl-cc:ast-if-p (cdr section)))
                           (cl-cc:ast-tagbody-tags tagbody)))
        (assert-true (some (lambda (section)
                             (some #'cl-cc:ast-go-p (cdr section)))
                           (cl-cc:ast-tagbody-tags tagbody)))))))

(deftest php-parser-break-continue-level-statements
  "break N and continue N lower to ast-go within nested loop tagbodies."
  (let ((ast (%php-first "<?php while ($a) { while ($b) { continue 2; break 2; } }")))
    (assert-true (cl-cc:ast-block-p ast))
    (let ((outer-tagbody (first (cl-cc:ast-block-body ast))))
      (assert-true (cl-cc:ast-tagbody-p outer-tagbody))
      (assert-true (some (lambda (section)
                           (some (lambda (form)
                                   (and (cl-cc:ast-block-p form)
                                        (cl-cc:ast-tagbody-p (first (cl-cc:ast-block-body form)))))
                                 (cdr section)))
                         (cl-cc:ast-tagbody-tags outer-tagbody))))))

(deftest php-parser-try-catch-finally-statement
  "Characterization: try/catch/finally should produce unwind-protect wrapping catch dispatch."
  (let ((ast (%php-first "<?php try { throw new Ex(); } catch (Ex $e) { echo $e; } finally { echo 'done'; }")))
    (assert-true (cl-cc:ast-unwind-protect-p ast))
    (let ((inner (cl-cc:ast-unwind-protected ast)))
      (assert-true (cl-cc:ast-let-p inner))
      (let ((dispatch (first (cl-cc:ast-let-body inner))))
        (assert-true (cl-cc:ast-if-p dispatch))))))

(deftest php-parser-catch-union-types
  "PHP catch clauses preserve union type alternatives for runtime dispatch."
  (let* ((ast (%php-first "<?php try { throw new ExA(); } catch (ExA | ExB $e) { echo $e; }"))
         (inner (cl-cc:ast-unwind-protected ast))
         (top-dispatch (first (cl-cc:ast-let-body inner)))
         (catch-dispatch (cl-cc:ast-if-then top-dispatch))
         (match-cond (cl-cc:ast-if-cond catch-dispatch))
         (class-arg (second (cl-cc:ast-call-args match-cond))))
    (assert-true (cl-cc:ast-call-p match-cond))
    (assert-equal '("EXA" "EXB")
                  (mapcar #'symbol-name (cl-cc:ast-quote-value class-arg)))))

(deftest php-parser-throw-statement
  "Characterization: throw should parse as ast-throw with PHP exception payload metadata."
  (let ((ast (%php-first "<?php throw new Ex();")))
    (assert-true (cl-cc:ast-throw-p ast))
    (let ((tag-val (cl-cc:ast-quote-value (cl-cc:ast-throw-tag ast))))
      (assert-true (symbolp tag-val))
      (assert-true (search "EXCEPTION" (symbol-name tag-val))))
    (assert-true (cl-cc:ast-call-p (cl-cc:ast-throw-value ast)))
    (assert-string= "%PHP-MAKE-EXCEPTION" (%php-call-name (cl-cc:ast-throw-value ast)))))

(deftest php-parser-short-array-literal
  "Characterization: [1,2,3] should preserve an ordered PHP array literal node."
  (let ((value (%php-first-binding-value "<?php $xs = [1, 2, 3];")))
    (assert-true (cl-cc:ast-call-p value))
    (assert-string= "%PHP-ARRAY" (%php-call-name value))
    (assert-= 3 (length (cl-cc:ast-call-args value)))
    (assert-true (every #'cl-cc:ast-list-p (cl-cc:ast-call-args value)))))

(deftest php-parser-associative-array-literal
  "Characterization: [\"a\"=>1,\"b\"=>2] should preserve key/value pairs."
  (let ((value (%php-first-binding-value "<?php $map = [\"a\" => 1, \"b\" => 2];")))
    (assert-true (cl-cc:ast-call-p value))
    (assert-string= "%PHP-ARRAY" (%php-call-name value))
    (assert-= 2 (length (cl-cc:ast-call-args value)))
    (assert-true
     (every (lambda (entry)
              (and (cl-cc:ast-list-p entry)
                   (cl-cc:ast-quote-value (first (cl-cc:ast-list-elements entry)))))
            (cl-cc:ast-call-args value)))))

(deftest php-parser-array-function-style-literal
  "Characterization: array(1,2,3) should preserve PHP array literal semantics."
  (let ((value (%php-first-binding-value "<?php $xs = array(1, 2, 3);")))
    (assert-true (cl-cc:ast-call-p value))
    (assert-string= "%PHP-ARRAY" (%php-call-name value))
    (assert-= 3 (length (cl-cc:ast-call-args value)))))

(deftest php-parser-array-element-access
  "$a[0] lowers to %PHP-ARRAY-REF with exactly 2 arguments (array + index)."
  (let ((value (%php-first-binding-value "<?php $x = $a[0];")))
    (assert-true (cl-cc:ast-call-p value))
    (assert-string= "%PHP-ARRAY-REF" (%php-call-name value))
    (assert-= 2 (length (cl-cc:ast-call-args value)))))

(deftest php-parser-array-element-assignment
  "$a[0] = $v lowers to the ordered PHP array mutation helper."
  (let ((ast (%php-first "<?php $a[0] = $v;")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-string= "%PHP-ARRAY-SET" (%php-call-name ast))
    (assert-= 3 (length (cl-cc:ast-call-args ast)))))

(deftest php-parser-compound-assignment-variable
  "$x += 5 — after php-finish-let-bindings, $x=0 wraps $x+=5 in its body."
  (let* ((asts (cl-cc/php:parse-php-source "<?php $x = 0; $x += 5;"))
         ;; $x=0 let wraps $x+=5 in its body; $x+=5 is first in that body
         (compound (first (cl-cc:ast-let-body (first asts)))))
    (assert-true (cl-cc:ast-let-p compound))
    (let ((setq (first (cl-cc:ast-let-body compound))))
      (assert-true (cl-cc:ast-setq-p setq))
      (assert-string= "x" (symbol-name (cl-cc:ast-setq-var setq)))
      (let ((value (cl-cc:ast-setq-value setq)))
        (assert-true (cl-cc:ast-call-p value))
        (assert-string= "%PHP-ADD" (%php-call-name value))))))

(deftest php-parser-compound-assignment-array-element
  "$arr[0] += 1 lowers to an array-set around an array-ref read."
  (let ((ast (%php-first "<?php $arr[0] += 1;")))
    (assert-true (cl-cc:ast-let-p ast))
    (let ((set-call (first (cl-cc:ast-let-body ast))))
      (assert-true (cl-cc:ast-call-p set-call))
      (assert-string= "%PHP-ARRAY-SET" (%php-call-name set-call))
      (let ((value (third (cl-cc:ast-call-args set-call))))
        (assert-true (cl-cc:ast-call-p value))
        (assert-string= "%PHP-ADD" (%php-call-name value))
        (assert-string= "%PHP-ARRAY-REF"
                        (%php-call-name (first (cl-cc:ast-call-args value))))))))

(deftest php-parser-null-coalescing-assignment-variable
  "$x ??= 42 on a KNOWN variable lowers to a null-checking conditional
assignment.  ($x is pre-declared; on an undefined var ??= introduces it to the
RHS directly, a different and simpler shape.)"
  (let* ((outer (%php-first "<?php $x = 1; $x ??= 42;"))
         (ast (first (cl-cc:ast-let-body outer))))
    (assert-true (cl-cc:ast-let-p ast))
    (let ((if-node (first (cl-cc:ast-let-body ast))))
      (assert-true (cl-cc:ast-if-p if-node))
      (assert-true (cl-cc:ast-setq-p (cl-cc:ast-if-then if-node)))
      (assert-string= "x" (symbol-name (cl-cc:ast-setq-var (cl-cc:ast-if-then if-node)))))))

(deftest php-parser-compound-assignment-property
  "$obj->count *= 3 lowers to a slot write using the previous slot value."
  (let ((ast (%php-first "<?php $obj->count *= 3;")))
    (assert-true (cl-cc:ast-let-p ast))
    (let ((slot-set (first (cl-cc:ast-let-body ast))))
      (assert-true (cl-cc:ast-set-slot-value-p slot-set))
      (assert-string= "COUNT" (symbol-name (cl-cc:ast-set-slot-value-slot slot-set)))
      (let ((value (cl-cc:ast-set-slot-value-value slot-set)))
        (assert-true (cl-cc:ast-call-p value))
        (assert-string= "%PHP-MUL" (%php-call-name value))))))

(deftest php-parser-all-compound-assignment-operators-parse
  "Every PHP compound assignment operator on a KNOWN variable parses as a
read-modify-write form (a let whose body reads the old value and writes back).
$x is pre-declared so this exercises the read-modify-write path, not the
undefined-var introduce path."
  (dolist (op '("+=" "-=" "*=" "/=" ".=" "%=" "**=" "&=" "|=" "^=" "<<=" ">>=" "??="))
    ;; `$x = 1' lowers to (let ((x 1)) BODY); the compound form is BODY[0].
    (let* ((outer (%php-first (format nil "<?php $x = 1; $x ~A 2;" op)))
           (ast (first (cl-cc:ast-let-body outer))))
      (assert-true (cl-cc:ast-let-p ast))
      (assert-true (first (cl-cc:ast-let-body ast))))))

(deftest php-parser-unset-array-element-lowering
  "unset($a[0]) lowers to the ordered PHP array deletion helper."
  (let ((ast (%php-first "<?php unset($a[0]);")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-string= "%PHP-ARRAY-UNSET" (%php-call-name ast))
    (assert-= 2 (length (cl-cc:ast-call-args ast)))))

(deftest php-parser-unset-object-property-lowering
  "unset($o->x) lowers to a property write of PHP null."
  (let ((ast (%php-first "<?php unset($o->x);")))
    (assert-true (cl-cc:ast-set-slot-value-p ast))
    (assert-string= "X" (symbol-name (cl-cc:ast-set-slot-value-slot ast)))
    (assert-true (cl-cc:ast-quote-p (cl-cc:ast-set-slot-value-value ast)))))

(deftest php-parser-declare-block-keeps-body
  "declare(ticks=...) { ... } is a directive wrapper; the parser must keep the body."
  (let ((ast (%php-first "<?php declare(ticks=1) { echo 'a'; echo 'b'; }")))
    (assert-true (cl-cc:ast-progn-p ast))
    (assert-= 2 (length (cl-cc:ast-progn-forms ast)))
    (assert-true (every #'cl-cc:ast-call-p (cl-cc:ast-progn-forms ast)))))

(deftest php-parser-declare-alternative-keeps-body
  "declare(ticks=...): ... enddeclare; is a directive wrapper; the body remains ordered."
  (let ((ast (%php-first "<?php declare(ticks=1): echo 'a'; echo 'b'; enddeclare;")))
    (assert-true (cl-cc:ast-progn-p ast))
    (assert-= 2 (length (cl-cc:ast-progn-forms ast)))
    (assert-true (every #'cl-cc:ast-call-p (cl-cc:ast-progn-forms ast)))))

(deftest php-parser-close-tag-is-accepted
  "A closing ?> tag terminates PHP mode without becoming an expression token."
  (let ((asts (cl-cc/php:parse-php-source "<?php echo 1; ?>")))
    (assert-= 1 (length asts))
    (assert-true (cl-cc:ast-call-p (first asts)))
    (assert-string= "%PHP-OUTPUT-WRITE" (%php-call-name (first asts)))))

(deftest php-parser-inline-html-between-tags
  "Inline HTML after ?> lowers to verbatim, output-buffer-aware output before the
next PHP block."
  (let ((asts (cl-cc/php:parse-php-source "<?php echo 1; ?>hello<?php echo 2;")))
    (assert-= 3 (length asts))
    (assert-true (every (lambda (a)
                          (and (cl-cc:ast-call-p a)
                               (string= "%PHP-OUTPUT-WRITE" (%php-call-name a))))
                        asts))))

(deftest-each php-parser-array-spread-syntax
  "Array spread syntax parses without error."
  :cases (("spread-only"  "<?php $a = [...$b, ...$c];")
          ("spread-mixed" "<?php $a = [1, ...$b, 2];"))
  (src)
  (assert-true (%php-first src)))

(deftest-each php-parser-list-destructuring
  "[$a, $b, ...] = $arr lowers to ast-let with the correct binding count."
  :cases (("two-targets"   "<?php [$a, $b] = $arr;"        2)
          ("three-targets" "<?php [$x, $y, $z] = $data;"   3))
  (src expected-bindings)
  (let ((ast (%php-first src)))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-= expected-bindings (length (cl-cc:ast-let-bindings ast)))))


(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
