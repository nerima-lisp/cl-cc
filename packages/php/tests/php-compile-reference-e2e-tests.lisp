(in-package :cl-cc/test)


(it-sequential "php-e2e-foreach-over-array-literal"
  (expect (%php-run-capture
                       "<?php $s=0; foreach ([1,2,3] as $v) { $s=$s+$v; } echo $s;") :to-equal "6"))


(it-sequential "php-e2e-foreach-assoc-key-value"
  (expect (%php-run-capture
                   "<?php $o=''; foreach (['a'=>1,'b'=>2,'c'=>3] as $k=>$v){ $o=$o.$k.'='.$v.';'; } echo $o;") :to-equal "a=1;b=2;c=3;"))


(it-sequential "php-e2e-foreach-by-reference-mutates-array"
  (expect (%php-run-capture
                   "<?php $a=[1,2]; foreach ($a as &$v) { $v=$v*10; } echo $a[0].','.$a[1];") :to-equal "10,20")
  (expect (%php-run-capture
                   "<?php $a=[1=>2]; foreach ($a as $k=>&$v) { $v=$k+$v; } echo $a[1];") :to-equal "3"))


(it-sequential "php-e2e-by-reference-function-parameter"
  (expect (%php-run-capture
                   "<?php function inc(&$x) { $x=$x+1; } $n=1; inc($n); echo $n;") :to-equal "2"))


(it-sequential "php-e2e-by-reference-function-metadata-is-source-local"
  (expect (%php-run-capture
                   "<?php function f(&$x) { $x=$x+1; } $n=1; f($n); echo $n;") :to-equal "2")
  (expect (%php-run-capture
                   "<?php function f($a,$b=10) { return $a+$b; } echo f(5);") :to-equal "15")
  (expect (%php-run-capture
                   "<?php function f($a,...$rest) { return $a.':'.count($rest); } echo f('x',1,2,3);") :to-equal "x:3"))


(it-sequential "php-e2e-reference-assignment-aliases-variables"
  (expect (%php-run-capture
                   "<?php $a=1; $b=&$a; $a=2; echo $b;") :to-equal "2")
  (expect (%php-run-capture
                   "<?php $a=1; $b=&$a; $b=3; echo $a.','.$b;") :to-equal "3,3"))


(it-sequential "php-e2e-generator-yield-foreach"
  (expect (%php-run-capture
                       "<?php function g(){ yield 1; yield 2; yield 3; } $s=0; foreach (g() as $v){ $s=$s+$v; } echo $s;") :to-equal "6"))


(it-sequential "php-e2e-generator-parametrized"
  (expect (%php-run-capture
                        "<?php function c($n){ for($i=1;$i<=$n;$i++){ yield $i; } } $s=0; foreach (c(5) as $v){ $s=$s+$v; } echo $s;") :to-equal "15"))


(it-sequential "php-e2e-generator-yield-from"
  (expect (%php-run-capture
                   "<?php function inner(){ yield 1; yield 2; } function outer(){ yield 0; yield from inner(); yield 3; } $o=''; foreach (outer() as $v){ $o=$o.$v.','; } echo $o;") :to-equal "0,1,2,3,"))


(it-sequential "php-e2e-fiber-object-methods"
  (expect (%php-run-capture
                   "<?php $f = new Fiber(function(){ return 42; }); echo $f->start().':'.$f->getReturn().':'.($f->isTerminated()?1:0);") :to-equal "42:42:1"))


(it-sequential "php-e2e-fiber-suspend"
  (expect (%php-run-capture
                   "<?php $f = new Fiber(function(){ return Fiber::suspend('pause'); }); echo $f->start().':'.($f->isSuspended()?1:0);") :to-equal "pause:1"))


(it-sequential "php-e2e-dynamic-closure-call"
  (expect (%php-run-capture
                        "<?php $f = function($x){ return $x*10; }; echo $f(5);") :to-equal "50"))


(it-sequential "php-e2e-iife"
  (expect (%php-run-capture
                        "<?php echo (function($x){ return $x*2; })(5);") :to-equal "10"))


(it-sequential "php-e2e-closure-passed-to-user-function"
  (expect (%php-run-capture
                        "<?php function apply($f,$v){ return $f($v); } echo apply(function($x){ return $x*3; }, 4);") :to-equal "12"))


(it-sequential "php-e2e-by-reference-parameters-literal-callables"
  (expect (%php-run-capture "<?php $n=1; (function (&$x) { $x = $x + 4; })($n); echo $n;") :to-equal "5")
  (expect (%php-run-capture "<?php $n=2; (fn(&$x)=>++$x)($n); echo $n;") :to-equal "3"))
