(in-package :cl-cc/test)

(in-suite cl-cc-php-e2e-suite)

(deftest php-e2e-foreach-over-array-literal
  "foreach over a PHP array literal iterates its values (regression: the non-key
foreach path used to assume a CL list and errored on the hash-table array)."
  (assert-string= "6" (%php-run-capture
                       "<?php $s=0; foreach ([1,2,3] as $v) { $s=$s+$v; } echo $s;")))


(deftest php-e2e-foreach-assoc-key-value
  "foreach ($arr as $k => $v) yields associative keys and values in order."
  (assert-string= "a=1;b=2;c=3;"
                  (%php-run-capture
                   "<?php $o=''; foreach (['a'=>1,'b'=>2,'c'=>3] as $k=>$v){ $o=$o.$k.'='.$v.';'; } echo $o;")))


(deftest php-e2e-foreach-by-reference-mutates-array
  "foreach ($arr as &$v) writes body mutations back to the iterated array."
  (assert-string= "10,20"
                  (%php-run-capture
                   "<?php $a=[1,2]; foreach ($a as &$v) { $v=$v*10; } echo $a[0].','.$a[1];"))
  (assert-string= "3"
                  (%php-run-capture
                   "<?php $a=[1=>2]; foreach ($a as $k=>&$v) { $v=$k+$v; } echo $a[1];")))


(deftest php-e2e-by-reference-function-parameter
  "A user function declared with &$x can mutate the caller's variable."
  (assert-string= "2"
                  (%php-run-capture
                   "<?php function inc(&$x) { $x=$x+1; } $n=1; inc($n); echo $n;")))


(deftest php-e2e-by-reference-function-metadata-is-source-local
  "By-reference parameter metadata from one source must not affect later sources."
  (assert-string= "2"
                  (%php-run-capture
                   "<?php function f(&$x) { $x=$x+1; } $n=1; f($n); echo $n;"))
  (assert-string= "15"
                  (%php-run-capture
                   "<?php function f($a,$b=10) { return $a+$b; } echo f(5);"))
  (assert-string= "x:3"
                  (%php-run-capture
                   "<?php function f($a,...$rest) { return $a.':'.count($rest); } echo f('x',1,2,3);")))


(deftest php-e2e-reference-assignment-aliases-variables
  "$b = &$a aliases both variable names to the same PHP reference box."
  (assert-string= "2"
                  (%php-run-capture
                   "<?php $a=1; $b=&$a; $a=2; echo $b;"))
  (assert-string= "3,3"
                  (%php-run-capture
                   "<?php $a=1; $b=&$a; $b=3; echo $a.','.$b;")))


(deftest php-e2e-generator-yield-foreach
  "A function containing yield becomes a generator; foreach drains its values."
  (assert-string= "6" (%php-run-capture
                       "<?php function g(){ yield 1; yield 2; yield 3; } $s=0; foreach (g() as $v){ $s=$s+$v; } echo $s;")))


(deftest php-e2e-generator-parametrized
  "A parametrized generator with a loop body yields the expected sequence."
  (assert-string= "15" (%php-run-capture
                        "<?php function c($n){ for($i=1;$i<=$n;$i++){ yield $i; } } $s=0; foreach (c(5) as $v){ $s=$s+$v; } echo $s;")))


(deftest php-e2e-generator-yield-from
  "yield from delegates to a nested generator, splicing its values in place."
  (assert-string= "0,1,2,3,"
                  (%php-run-capture
                   "<?php function inner(){ yield 1; yield 2; } function outer(){ yield 0; yield from inner(); yield 3; } $o=''; foreach (outer() as $v){ $o=$o.$v.','; } echo $o;")))


(deftest php-e2e-fiber-object-methods
  "new Fiber(callback) returns an object whose instance methods dispatch via PHP syntax."
  (assert-string= "42:42:1"
                  (%php-run-capture
                   "<?php $f = new Fiber(function(){ return 42; }); echo $f->start().':'.$f->getReturn().':'.($f->isTerminated()?1:0);")))


(deftest php-e2e-fiber-suspend
  "Fiber::suspend(value) returns the first suspended value from start()."
  (assert-string= "pause:1"
                  (%php-run-capture
                   "<?php $f = new Fiber(function(){ return Fiber::suspend('pause'); }); echo $f->start().':'.($f->isSuspended()?1:0);")))


(deftest php-e2e-dynamic-closure-call
  "Calling a closure held in a variable, $f(args), invokes it (postfix LPAREN
lowers to a call on the value)."
  (assert-string= "50" (%php-run-capture
                        "<?php $f = function($x){ return $x*10; }; echo $f(5);")))


(deftest php-e2e-iife
  "An immediately-invoked function expression evaluates and calls in place."
  (assert-string= "10" (%php-run-capture
                        "<?php echo (function($x){ return $x*2; })(5);")))


(deftest php-e2e-closure-passed-to-user-function
  "A closure passed to a user function is callable inside it via $f(args)."
  (assert-string= "12" (%php-run-capture
                        "<?php function apply($f,$v){ return $f($v); } echo apply(function($x){ return $x*3; }, 4);")))


(deftest php-e2e-by-reference-parameters-literal-callables
  "Anonymous and arrow function literals preserve by-reference parameters for
immediate calls."
  (assert-string= "5" (%php-run-capture "<?php $n=1; (function (&$x) { $x = $x + 4; })($n); echo $n;"))
  (assert-string= "3" (%php-run-capture "<?php $n=2; (fn(&$x)=>++$x)($n); echo $n;")))
