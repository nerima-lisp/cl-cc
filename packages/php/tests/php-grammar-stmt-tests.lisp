(in-package :cl-cc/test)


(defun %php-cst-parse-one (source)
  "Parse SOURCE into CST forms and diagnostics, returning both."
  (multiple-value-list (cl-cc/php:parse-php-source-to-cst source)))

(it-sequential "php-grammar-stmt-basic-dispatch echo"
  (destructuring-bind (source) (list "<?php echo 1;")
    (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one source)
    (expect (consp forms) :to-be-truthy)
    (expect diagnostics :to-equal nil))))

(it-sequential "php-grammar-stmt-basic-dispatch return"
  (destructuring-bind (source) (list "<?php return 1;")
    (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one source)
    (expect (consp forms) :to-be-truthy)
    (expect diagnostics :to-equal nil))))

(it-sequential "php-grammar-stmt-basic-dispatch if-else"
  (destructuring-bind (source) (list "<?php if ($x) { echo 1; } else { echo 2; }")
    (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one source)
    (expect (consp forms) :to-be-truthy)
    (expect diagnostics :to-equal nil))))

(it-sequential "php-grammar-stmt-basic-dispatch while"
  (destructuring-bind (source) (list "<?php while ($x) { echo 1; }")
    (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one source)
    (expect (consp forms) :to-be-truthy)
    (expect diagnostics :to-equal nil))))

(it-sequential "php-grammar-stmt-basic-dispatch foreach"
  (destructuring-bind (source) (list "<?php foreach ($items as $item) { echo $item; }")
    (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one source)
    (expect (consp forms) :to-be-truthy)
    (expect diagnostics :to-equal nil))))

(it-sequential "php-grammar-stmt-basic-dispatch function"
  (destructuring-bind (source) (list "<?php function greet($name) { return $name; }")
    (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one source)
    (expect (consp forms) :to-be-truthy)
    (expect diagnostics :to-equal nil))))

(it-sequential "php-grammar-stmt-basic-dispatch class"
  (destructuring-bind (source) (list "<?php class Box extends Base { public function get($x) { return $x; } }")
    (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one source)
    (expect (consp forms) :to-be-truthy)
    (expect diagnostics :to-equal nil))))

(it-sequential "php-grammar-stmt-basic-dispatch try-catch"
  (destructuring-bind (source) (list "<?php try { echo 1; } catch (Ex $e) { echo 2; }")
    (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one source)
    (expect (consp forms) :to-be-truthy)
    (expect diagnostics :to-equal nil))))

(it-sequential "php-grammar-stmt-basic-dispatch break"
  (destructuring-bind (source) (list "<?php break;")
    (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one source)
    (expect (consp forms) :to-be-truthy)
    (expect diagnostics :to-equal nil))))

(it-sequential "php-grammar-stmt-basic-dispatch continue"
  (destructuring-bind (source) (list "<?php continue;")
    (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one source)
    (expect (consp forms) :to-be-truthy)
    (expect diagnostics :to-equal nil))))

(it-sequential "php-grammar-stmt-expression-fallback"
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php $x + 1;")
    (expect (= 1 (length forms)) :to-be-truthy)
    (expect (first forms) :to-be-truthy)
    (expect diagnostics :to-equal nil)))

;;; P1+P2: for and foreach parsers
(it-sequential "php-grammar-stmt-for-loop-has-five-children"
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php for ($i = 0; $i < 10; $i = $i + 1) { echo $i; }")
    (expect diagnostics :to-equal nil)
    (expect (= 1 (length forms)) :to-be-truthy)
    (let ((node (first forms)))
      (expect (cl-cc:cst-node-kind node) :to-be :for)
      (expect (= 5 (length (cl-cc:cst-interior-children node))) :to-be-truthy)
      (let ((body-node (fifth (cl-cc:cst-interior-children node))))
        (expect (cl-cc:cst-node-kind body-node) :to-be :body)))))

(it-sequential "php-grammar-stmt-foreach-key-value-has-four-children"
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php foreach ($arr as $k => $v) { echo $k; }")
    (expect diagnostics :to-equal nil)
    (expect (= 1 (length forms)) :to-be-truthy)
    (let ((node (first forms)))
      (expect (cl-cc:cst-node-kind node) :to-be :foreach)
      (expect (= 4 (length (cl-cc:cst-interior-children node))) :to-be-truthy))))

;;; P3a+b: function return type and param defaults
(it-sequential "php-grammar-stmt-function-with-return-type"
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php function add($a, $b): int { return $a + $b; }")
    (expect diagnostics :to-equal nil)
    (expect (= 1 (length forms)) :to-be-truthy)
    (expect (cl-cc:cst-node-kind (first forms)) :to-be :function-def)))

(it-sequential "php-grammar-stmt-function-with-default-param"
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php function greet($name = \"world\") { return $name; }")
    (expect diagnostics :to-equal nil)
    (expect (= 1 (length forms)) :to-be-truthy)
    (expect (cl-cc:cst-node-kind (first forms)) :to-be :function-def)))

;;; P3c: class implements — single and multiple
(it-sequential "php-grammar-stmt-class-single-implements"
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php class Box implements Iface { public function get($x) { return $x; } }")
    (expect diagnostics :to-equal nil)
    (expect (= 1 (length forms)) :to-be-truthy)
    (expect (cl-cc:cst-node-kind (first forms)) :to-be :class-def)))

(it-sequential "php-grammar-stmt-class-multiple-implements"
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php class Box implements IfaceA, IfaceB { public $x; }")
    (expect diagnostics :to-equal nil)
    (expect (= 1 (length forms)) :to-be-truthy)
    (expect (cl-cc:cst-node-kind (first forms)) :to-be :class-def)))

;;; P4: structural assertions on simple statements
(it-sequential "php-grammar-stmt-echo-has-two-children"
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php echo 42;")
    (expect diagnostics :to-equal nil)
    (expect (= 1 (length forms)) :to-be-truthy)
    (let ((node (first forms)))
      (expect (cl-cc:cst-node-kind node) :to-be :echo)
      (expect (= 2 (length (cl-cc:cst-interior-children node))) :to-be-truthy))))

(it-sequential "php-grammar-stmt-return-with-value-has-two-children"
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php return 42;")
    (expect diagnostics :to-equal nil)
    (let ((node (first forms)))
      (expect (cl-cc:cst-node-kind node) :to-be :return)
      (expect (= 2 (length (cl-cc:cst-interior-children node))) :to-be-truthy))))

(it-sequential "php-grammar-stmt-bare-return-has-one-child"
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php return;")
    (expect diagnostics :to-equal nil)
    (let ((node (first forms)))
      (expect (cl-cc:cst-node-kind node) :to-be :return)
      (expect (= 1 (length (cl-cc:cst-interior-children node))) :to-be-truthy))))

(it-sequential "php-grammar-stmt-if-only-has-then-no-else"
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php if ($x) { echo 1; }")
    (expect diagnostics :to-equal nil)
    (let ((node (first forms)))
      (expect (cl-cc:cst-node-kind node) :to-be :if)
      (let ((kinds (mapcar #'cl-cc:cst-node-kind
                           (remove-if-not #'cl-cc:cst-interior-p
                                          (cl-cc:cst-interior-children node)))))
        (expect (member :then kinds) :to-be-truthy)
        (expect (member :else kinds) :to-be-falsy)))))

(it-sequential "php-grammar-stmt-if-else-has-both-then-and-else"
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php if ($x) { echo 1; } else { echo 2; }")
    (expect diagnostics :to-equal nil)
    (let ((node (first forms)))
      (expect (cl-cc:cst-node-kind node) :to-be :if)
      (let ((kinds (mapcar #'cl-cc:cst-node-kind
                           (remove-if-not #'cl-cc:cst-interior-p
                                          (cl-cc:cst-interior-children node)))))
        (expect (member :then kinds) :to-be-truthy)
        (expect (member :else kinds) :to-be-truthy)))))

(it-sequential "php-grammar-stmt-try-catch-structure"
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php try { echo 1; } catch (Ex $e) { echo 2; }")
    (expect diagnostics :to-equal nil)
    (let ((node (first forms)))
      (expect (cl-cc:cst-node-kind node) :to-be :try-catch)
      (let ((kinds (mapcar #'cl-cc:cst-node-kind
                           (remove-if-not #'cl-cc:cst-interior-p
                                          (cl-cc:cst-interior-children node)))))
        (expect (member :try-body kinds) :to-be-truthy)
        (expect (member :catch kinds) :to-be-truthy)))))

(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
