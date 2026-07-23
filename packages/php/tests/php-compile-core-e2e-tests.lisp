(in-package :cl-cc/test)


(it-sequential "php-e2e-postfix-incdec-on-places"
  (expect (%php-run-capture "<?php class C{ public $n=0; } $o=new C(); $o->n++; $o->n++; echo $o->n;") :to-equal "2")
  (expect (%php-run-capture "<?php class C{ public $n=10; } $o=new C(); echo $o->n++;") :to-equal "10")
  (expect (%php-run-capture "<?php class C{ public $n=5; } $o=new C(); $o->n--; echo $o->n;") :to-equal "4")
  (expect (%php-run-capture "<?php class C{ public $n=0; function inc(){ $this->n++; } } $o=new C(); $o->inc(); $o->inc(); $o->inc(); echo $o->n;") :to-equal "3")
  (expect (%php-run-capture "<?php $a=[0]; $a[0]++; $a[0]++; echo $a[0];") :to-equal "2")
  (expect (%php-run-capture "<?php $a=[5]; $old=$a[0]++; echo $old.'-'.$a[0];") :to-equal "5-6")
  (expect (%php-run-capture "<?php $x='7'; $old=$x++; echo $old.'-'.$x;") :to-equal "7-8")
  (expect (%php-run-capture "<?php $x=null; ++$x; echo $x;") :to-equal "1")
  (expect (%php-run-capture "<?php $x=true; --$x; echo $x;") :to-equal "0"))


(it-sequential "php-e2e-incdec-undefined-variable"
  (expect (%php-run-capture "<?php $x++; echo $x;") :to-equal "1")
  (expect (%php-run-capture "<?php echo (++$x).':'.$x;") :to-equal "1:1")
  (expect (%php-run-capture "<?php $x--; echo $x;") :to-equal "-1")
  (expect (%php-run-capture "<?php echo (--$x).':'.$x;") :to-equal "-1:-1"))


(it-sequential "php-e2e-echo-no-trailing-newline-multi"
  (expect (%php-run-capture "<?php echo 'a'; echo 'b'; echo 'c';") :to-equal "abc"))


(it-sequential "php-e2e-echo-no-trailing-newline-multiarg"
  (expect (%php-run-capture "<?php echo 'x', 'y', 'z';") :to-equal "xyz"))


(it-sequential "php-e2e-echo-interior-newline-preserved"
  (expect (%php-run-capture "<?php echo \"p\\n\"; echo 'q';") :to-equal (format nil "p~%q")))


(it-sequential "php-e2e-inline-html-verbatim"
  (expect (%php-run-capture "<?php echo 1; ?>hello<?php echo 2;") :to-equal "1hello2"))


(it-sequential "php-e2e-declare-directives"
  (expect (%php-run-capture "<?php declare(strict_types=1); echo 7;") :to-equal "7")
  (expect (%php-run-capture "<?php declare(ticks=1) { echo 'A'; echo 'B'; } echo 'C';") :to-equal "ABC")
  (expect (%php-run-capture "<?php declare(ticks=1) echo 'X'; echo 'Y';") :to-equal "XY")
  (expect (%php-run-capture "<?php declare(ticks=1): echo 'A'; echo 'B'; enddeclare; echo 'C';") :to-equal "ABC"))


(it-sequential "php-e2e-output-buffering-builtins"
  (expect (%php-run-capture "<?php ob_start(); echo 'a'; $s=ob_get_clean(); echo $s; echo 'b';") :to-equal "ab")
  (expect (%php-run-capture "<?php ob_start(); echo 'x'; $s=ob_get_contents(); ob_end_clean(); echo $s.'y';") :to-equal "xy")
  (expect (%php-run-capture "<?php ob_start(); echo 'a'; ob_start(); echo 'x'; ob_end_clean(); echo 'b'; echo ob_get_clean();") :to-equal "ab")
  (expect (%php-run-capture "<?php ob_start(); ?>html<?php $s=ob_get_clean(); echo $s.'!';") :to-equal "html!"))


(it-sequential "php-e2e-ini-get-set-and-error-reporting"
  (expect (%php-run-capture "<?php echo ini_get('default_charset');") :to-equal "UTF-8")
  (expect (%php-run-capture "<?php $old=ini_set('default_charset','Shift_JIS'); echo $old.':'.ini_get('default_charset'); ini_set('default_charset',$old);") :to-equal "UTF-8:Shift_JIS")
  (expect (%php-run-capture "<?php error_reporting(32767); $old=error_reporting(0); echo $old.':'.error_reporting().':'.ini_get('error_reporting'); error_reporting($old);") :to-equal "32767:0:0")
  (expect (%php-run-capture "<?php error_reporting(32767); $old=ini_set('error_reporting','5'); echo $old.':'.error_reporting().':'.ini_get('error_reporting'); error_reporting($old);") :to-equal "32767:5:5")
  (expect (%php-run-capture "<?php echo ini_get('definitely_missing')===false?'false':'value';") :to-equal "false"))


(it-sequential "php-e2e-error-and-exception-handler-registration"
  (expect (%php-run-capture "<?php function h($errno,$errstr,$file,$line){ echo 'H:'.$errno.':'.$errstr; return true; } set_error_handler('h', E_USER_WARNING); trigger_error('boom', E_USER_WARNING); restore_error_handler();") :to-equal "H:512:boom")
  (expect (%php-run-capture "<?php function only_notice($errno,$errstr){ echo 'handled'; return true; } set_error_handler('only_notice', E_USER_NOTICE); trigger_error('masked', E_USER_WARNING); restore_error_handler();") :to-equal "masked")
  (expect (%php-run-capture "<?php function h1($errno,$errstr){ echo 'one'; return true; } function h2($errno,$errstr){ echo 'two'; return true; } set_error_handler('h1'); $old=set_error_handler('h2'); echo $old.':'; trigger_error('x', E_USER_NOTICE); restore_error_handler(); restore_error_handler();") :to-equal "h1:two")
  (expect (%php-run-capture "<?php function eh($e){ echo 'e'; } set_exception_handler('eh'); $old=set_exception_handler(function($e){ echo 'x'; }); echo $old.':ok'; restore_exception_handler(); restore_exception_handler();") :to-equal "eh:ok"))


(it-sequential "php-e2e-error-suppression-operator"
  (expect (%php-run-capture "<?php @trigger_error('hidden', E_USER_NOTICE); echo 'ok';") :to-equal "ok")
  (expect (%php-run-capture "<?php function h($errno,$errstr){ echo 'handler'; return true; } set_error_handler('h'); @trigger_error('hidden', E_USER_NOTICE); restore_error_handler(); echo 'ok';") :to-equal "ok")
  (expect (%php-run-capture "<?php error_reporting(E_ALL); $v=@trigger_error('hidden', E_USER_NOTICE); echo $v.':'.error_reporting();") :to-equal "1:32767"))


(it-sequential "php-e2e-date-default-timezone-and-ini"
  (expect (%php-run-capture "<?php $old=date_default_timezone_get(); date_default_timezone_set('UTC'); $prev=date_default_timezone_get(); date_default_timezone_set('Asia/Tokyo'); echo $prev.':'.date_default_timezone_get().':'.ini_get('date.timezone').':'.date('H:i',0).':'.gmdate('H:i',0).':'.date('U',0); date_default_timezone_set($old);") :to-equal "UTC:Asia/Tokyo:Asia/Tokyo:09:00:00:00:0")
  (expect (%php-run-capture "<?php $old=date_default_timezone_get(); date_default_timezone_set('Asia/Tokyo'); $prev=ini_set('date.timezone','UTC'); echo $prev.':'.date_default_timezone_get().':'.ini_get('date.timezone'); date_default_timezone_set($old);") :to-equal "Asia/Tokyo:UTC:UTC")
  (expect (%php-run-capture "<?php $old=date_default_timezone_get(); date_default_timezone_set('UTC'); echo (date_default_timezone_set('No/Such_Zone')?'true':'false').':'.date_default_timezone_get(); date_default_timezone_set($old);") :to-equal "false:UTC")
  (expect (%php-run-capture "<?php $old=date_default_timezone_get(); date_default_timezone_set('Asia/Tokyo'); echo strtotime('1970-01-01 09:00:00').':'.mktime(18,0,0,1,1,1970).':'.date('Y-m-d H:i',0); date_default_timezone_set($old);") :to-equal "0:32400:1970-01-01 09:00"))


(it-sequential "php-e2e-heredoc-nowdoc"
  (expect (%php-run-capture (format nil "<?php $n=\"Bob\"; echo <<<EOT~%Hi $n~%EOT;~%")) :to-equal "Hi Bob")
  (expect (%php-run-capture (format nil "<?php $a=\"X\";$b=\"Y\"; echo <<<EOT~%$a-$b~%EOT;~%")) :to-equal "X-Y")
  (expect (%php-run-capture (format nil "<?php $n=\"Bob\"; echo <<<EOT~%Hi {$n}!~%EOT;~%")) :to-equal "Hi Bob!")
  (expect (%php-run-capture (format nil "<?php $n=\"Z\"; echo <<<EOT~%line1~%val=$n~%EOT;~%")) :to-equal (format nil "line1~%val=Z"))
  (expect (%php-run-capture (format nil "<?php $n=\"Q\"; echo <<<\"EOT\"~%v=$n~%EOT;~%")) :to-equal "v=Q")
  (expect (%php-run-capture (format nil "<?php $n=\"Bob\"; echo <<<'EOT'~%Hi $n~%EOT;~%")) :to-equal "Hi $n"))


(it-sequential "php-e2e-list-destructuring"
  (expect (%php-run-capture "<?php list($a,$b)=[1,2]; echo $a+$b;") :to-equal "3")
  (expect (%php-run-capture "<?php list($a,$b,$c)=[10,20,30]; echo $a+$b+$c;") :to-equal "60")
  (expect (%php-run-capture "<?php function pair(){return [4,9];} list($x,$y)=pair(); echo $x.'-'.$y;") :to-equal "4-9")
  (expect (%php-run-capture "<?php list($n,$a)=['Bob',30]; echo $n.':'.$a;") :to-equal "Bob:30")
  (expect (%php-run-capture "<?php [$a,$b]=[1,2]; echo $a.','.$b;") :to-equal "1,2")
  (expect (%php-run-capture "<?php $a=1;$b=2; [$a,$b]=[$b,$a]; echo $a.','.$b;") :to-equal "2,1"))


(it-sequential "php-e2e-list-destructuring-nested-keyed"
  (expect (%php-run-capture "<?php [[$a,$b],$c]=[[1,2],3]; echo $a+$b+$c;") :to-equal "6")
  (expect (%php-run-capture "<?php list(list($a,$b),$c)=[[1,2],3]; echo $a+$b+$c;") :to-equal "6")
  (expect (%php-run-capture "<?php ['x'=>$a,'y'=>$b]=['x'=>1,'y'=>2]; echo $a+$b;") :to-equal "3")
  (expect (%php-run-capture "<?php ['y'=>$a,'x'=>$b]=['x'=>1,'y'=>2]; echo $a-$b;") :to-equal "1")
  (expect (%php-run-capture "<?php [['k'=>$a],$b]=[['k'=>9],3]; echo $a.','.$b;") :to-equal "9,3"))


(it-sequential "php-e2e-complex-string-interpolation"
  (expect (%php-run-capture "<?php $a=['k'=>5]; echo \"v={$a['k']}\";") :to-equal "v=5")
  (expect (%php-run-capture "<?php $a=['x'=>['y'=>9]]; echo \"n={$a['x']['y']}\";") :to-equal "n=9")
  (expect (%php-run-capture "<?php class C{ function g(){ return 42; } } $o=new C(); echo \"m={$o->g()}\";") :to-equal "m=42")
  (expect (%php-run-capture "<?php $n='Bob'; $a=['age'=>30]; echo \"Hi $n, age {$a['age']}!\";") :to-equal "Hi Bob, age 30!")
  (expect (%php-run-capture "<?php $n='Bob'; echo \"Hi $n!\";") :to-equal "Hi Bob!")
  (expect (%php-run-capture "<?php $n='Bob'; echo \"Hi {$n}!\";") :to-equal "Hi Bob!"))


(it-sequential "php-e2e-equality-operators"
  (expect (%php-run-capture "<?php function f($x){ return $x == 4; } echo f(4);") :to-equal "1")
  (expect (%php-run-capture "<?php function f($x){ return $x == 4; } echo f(3);") :to-equal "")
  (expect (%php-run-capture "<?php function f($x){ return $x != 4; } echo f(3);") :to-equal "1")
  (expect (%php-run-capture "<?php function f($x){ return $x % 2 == 0; } echo f(4);") :to-equal "1"))


(it-sequential "php-e2e-equality-type-juggling"
  (expect (%php-run-capture "<?php $a='5'; if ($a == 5) { echo 'loose'; }") :to-equal "loose")
  (expect (%php-run-capture "<?php $a='5'; if ($a === 5) { echo 'x'; } else { echo 'strictfail'; }") :to-equal "strictfail"))


(it-sequential "php-e2e-echo-boolean-php-semantics"
  (expect (%php-run-capture "<?php echo true;") :to-equal "1")
  (expect (%php-run-capture "<?php echo false;") :to-equal "")
  (expect (%php-run-capture "<?php echo true . 'X';") :to-equal "1X")
  (expect (%php-run-capture "<?php echo false . 'X';") :to-equal "X")
  (expect (%php-run-capture "<?php echo (5 == 5);") :to-equal "1")
  (expect (%php-run-capture "<?php echo 1,2,3;") :to-equal "123"))


(it-sequential "php-e2e-logical-operators"
  (expect (%php-run-capture "<?php echo (true && true) ? 'T':'F';") :to-equal "T")
  (expect (%php-run-capture "<?php echo (true && false) ? 'T':'F';") :to-equal "F")
  (expect (%php-run-capture "<?php echo (false || true) ? 'T':'F';") :to-equal "T")
  (expect (%php-run-capture "<?php echo (false || false) ? 'T':'F';") :to-equal "F")
  (expect (%php-run-capture "<?php echo !false ? 'T':'F';") :to-equal "T")
  (expect (%php-run-capture "<?php echo !5 ? 'T':'F';") :to-equal "F")
  (expect (%php-run-capture "<?php echo true && true;") :to-equal "1"))


(it-sequential "php-e2e-logical-in-conditions"
  (expect (%php-run-capture "<?php $a=1; $b=2; if ($a && $b) { echo 'both'; }") :to-equal "both")
  (expect (%php-run-capture "<?php $x=5; if ($x > 0 && $x < 10) { echo 'mid'; }") :to-equal "mid")
  (expect (%php-run-capture "<?php $a=true; $b=false; if (!$b && $a) { echo 'ok'; }") :to-equal "ok"))


(it-sequential "php-e2e-logical-short-circuit"
  (expect (%php-run-capture
                   "<?php function boom(){ return 1/0; } $x=false; echo ($x && boom()) ? 'T':'F';") :to-equal "F"))


(it-sequential "php-e2e-default-arguments"
  (expect (%php-run-capture "<?php function g($x=7){ return $x; } echo g();") :to-equal "7")
  (expect (%php-run-capture "<?php function g($x=7){ return $x; } echo g(3);") :to-equal "3")
  (expect (%php-run-capture "<?php function g($s='hi'){ return $s; } echo g();") :to-equal "hi")
  (expect (%php-run-capture "<?php function f($a,$b=10){ return $a+$b; } echo f(5);") :to-equal "15")
  (expect (%php-run-capture "<?php function f($a,$b=10){ return $a+$b; } echo f(5,2);") :to-equal "7")
  (expect (%php-run-capture "<?php function f($a=1,$b=2,$c=3){ return $a+$b+$c; } echo f();") :to-equal "6")
  (expect (%php-run-capture "<?php function f($a=1,$b=2,$c=3){ return $a+$b+$c; } echo f(10);") :to-equal "15"))


(it-sequential "php-e2e-named-arguments"
  (expect (%php-run-capture
                   "<?php function join3($a,$b,$c){ return $a.$b.$c; } echo join3(c:'C', a:'A', b:'B');") :to-equal "ABC")
  (expect (%php-run-capture
                   "<?php function f($a='A',$b='B',$c='C'){ return $a.$b.$c; } echo f(c:'C', b:2);") :to-equal "A2C")
  (expect (%php-run-capture
                   "<?php function f($a='A',$b='B',$c='C'){ return $a.$b.$c; } echo f('X', c:'C');") :to-equal "XBC")
  (expect (%php-run-capture
                   "<?php function join3($a,$b,$c){ return $a.$b.$c; } echo join3(...['A'], b:'B', c:'C');") :to-equal "ABC")
  (expect (%php-run-capture
                   "<?php function join3($a,$b,$c){ return $a.$b.$c; } echo join3(...['A','B'], c:'C');") :to-equal "ABC")
  (expect (%php-run-capture
                   "<?php function f($a='A',$b='B',$c='C'){ return $a.$b.$c; } echo f(...['X'], c:'C');") :to-equal "XBC")
  (expect (%php-run-capture
                   "<?php class C{ function join3($a,$b,$c){ return $a.$b.$c; } } $c=new C(); echo $c->join3(c:'C', a:'A', b:'B');") :to-equal "ABC")
  (expect (%php-run-capture
                   "<?php class C{ function join3($a,$b,$c){ return $a.$b.$c; } } $c=new C(); echo $c?->join3(c:'C', a:'A', b:'B');") :to-equal "ABC")
  (expect (%php-run-capture
                   "<?php class C{ static function join3($a,$b,$c){ return $a.$b.$c; } } echo C::join3(c:'C', a:'A', b:'B');") :to-equal "ABC")
  (expect (%php-run-capture
                   "<?php class C{ function join3($a,$b,$c){ return $a.$b.$c; } } $c=new C(); echo $c->join3(...['A'], b:'B', c:'C');") :to-equal "ABC"))


(it-sequential "php-e2e-default-arguments-all-callable-forms"
  (expect (%php-run-capture "<?php $f=function($x=9){ return $x; }; echo $f();") :to-equal "9")
  (expect (%php-run-capture "<?php $f=function($x=9){ return $x; }; echo $f(4);") :to-equal "4")
  (expect (%php-run-capture "<?php $f=fn($x=8)=>$x*2; echo $f();") :to-equal "16")
  (expect (%php-run-capture "<?php class C{ function m($x=5){ return $x; } } $c=new C(); echo $c->m();") :to-equal "5")
  (expect (%php-run-capture "<?php class C{ function m($x=5){ return $x; } } $c=new C(); echo $c->m(20);") :to-equal "20"))


(it-sequential "php-e2e-variadic-parameters"
  (expect (%php-run-capture "<?php function s(...$n){ return array_sum($n); } echo s(1,2,3,4);") :to-equal "10")
  (expect (%php-run-capture "<?php function s(...$n){ return count($n); } echo s(1,2,3,4);") :to-equal "4")
  (expect (%php-run-capture "<?php function s(...$n){ return count($n); } echo s();") :to-equal "0")
  (expect (%php-run-capture "<?php function f($a,...$rest){ return $a.':'.count($rest); } echo f('x',1,2,3);") :to-equal "x:3")
  (expect (%php-run-capture "<?php function j(...$xs){ $r=''; foreach($xs as $x){ $r.=$x; } return $r; } echo j('a','b','c');") :to-equal "abc"))


(it-sequential "php-e2e-variadic-all-callable-forms"
  (expect (%php-run-capture "<?php $s=function(...$n){ return array_sum($n); }; echo $s(1,2,3);") :to-equal "6")
  (expect (%php-run-capture "<?php $s=fn(...$n)=>array_sum($n); echo $s(5,5,5);") :to-equal "15"))


(it-sequential "php-e2e-spread-call"
  (expect (%php-run-capture "<?php function add($a,$b){ return $a+$b; } $args=[2,3]; echo add(...$args);") :to-equal "5")
  (expect (%php-run-capture "<?php function add3($a,$b,$c){ return $a+$b+$c; } $x=[1,2,3]; echo add3(...$x);") :to-equal "6")
  (expect (%php-run-capture "<?php function f($a,$b,$c){ return $a.$b.$c; } $r=['y','z']; echo f('x',...$r);") :to-equal "xyz")
  (expect (%php-run-capture "<?php $a=[3,1,2]; echo max(...$a);") :to-equal "3")
  (expect (%php-run-capture "<?php $a=[3,1,2]; echo min(...$a);") :to-equal "1"))


(it-sequential "php-e2e-named-args-after-dynamic-spread"
  (expect (%php-run-capture
                   "<?php function j($a,$b,$c){ return $a.$b.$c; } $args=['a','b']; echo j(...$args, c:'c');") :to-equal "abc")
  (expect (%php-run-capture
                   "<?php function j($a,$b='x',$c='z'){ return $a.$b.$c; } $args=['a']; echo j(...$args, c:'c');") :to-equal "axc"))


(it-sequential "php-e2e-spread-composes-with-variadic-and-closures"
  (expect (%php-run-capture "<?php function s(...$n){ return array_sum($n); } $a=[10,20,30]; echo s(...$a);") :to-equal "60")
  (expect (%php-run-capture "<?php $f=function($a,$b){ return $a*$b; }; $args=[4,5]; echo $f(...$args);") :to-equal "20"))


(it-sequential "php-e2e-for-loop-continue"
  (expect (%php-run-capture "<?php $s=0; for($i=0;$i<6;$i++){ if($i%2)continue; $s+=$i; } echo $s;") :to-equal "6")
  (expect (%php-run-capture "<?php $s=0; for($i=0;$i<10;$i++){ if($i==5)break; if($i%2)continue; $s+=$i; } echo $s;") :to-equal "6")
  (expect (%php-run-capture "<?php $s=0; for($i=0;$i<3;$i++){ for($j=0;$j<3;$j++){ if($j==1)continue; $s++; } } echo $s;") :to-equal "6"))


(it-sequential "php-e2e-for-loop-basics"
  (expect (%php-run-capture "<?php $s=0; for($i=1;$i<=4;$i++){ $s+=$i; } echo $s;") :to-equal "10")
  (expect (%php-run-capture "<?php $s=0; for($i=0;$i<10;$i++){ if($i==3)break; $s+=$i; } echo $s;") :to-equal "3")
  (expect (%php-run-capture "<?php $s=''; for($i=3;$i>0;$i--){ $s.=$i; } echo $s;") :to-equal "321"))


(it-sequential "php-parser-cli-compile-path-for-php-files"
  (let* ((tmp-dir (uiop:temporary-directory))
         (input (merge-pathnames "cl-cc-php-compile-gap.php" tmp-dir))
         (output (merge-pathnames "cl-cc-php-compile-gap" tmp-dir)))
    (with-open-file (stream input :direction :output :if-exists :supersede)
      (write-line "<?php echo match($x) { 1 => 'one', default => 'other' };" stream))
    (let ((result (cl-cc::compile-file-to-native input :output-file output)))
      (expect result :to-equal output)
      (expect (probe-file output) :to-be-truthy))))


(it-sequential "php-runtime-expression-operator-helpers"
  (expect (= 3 (cl-cc/php:%php-modulo 7 4)) :to-be-truthy)
  (expect (= -1 (cl-cc/php:%php-modulo -7 3)) :to-be-truthy)
  (expect (= 8 (cl-cc/php:%php-shift-left 1 3)) :to-be-truthy)
  (expect (= -4 (cl-cc/php:%php-shift-right -8 1)) :to-be-truthy)
  (expect (= -1 (cl-cc/php:%php-spaceship 1 2)) :to-be-truthy)
  (expect (= 0 (cl-cc/php:%php-spaceship "2" 2)) :to-be-truthy)
  (expect (= 2 (cl-cc/php:%php-bitwise-and 6 3)) :to-be-truthy)
  (expect (= 7 (cl-cc/php:%php-bitwise-or 6 3)) :to-be-truthy)
  (expect (= 5 (cl-cc/php:%php-bitwise-xor 6 3)) :to-be-truthy)
  (expect (= -2 (cl-cc/php:%php-bitwise-not 1)) :to-be-truthy))


(it-sequential "php-compile-expression-operators"
  (let ((result (cl-cc:compile-string
                 "<?php $a = 2 ** 3 ** 2; $b = 7 % 4; $c = 1 << 3; $d = 8 >> 1; $e = 1 <=> 2; $f = 6 & 3; $g = 6 ^ 3; $h = 4 | 1; $i = ~1; $j = 1 + 2 . 3;"
                 :target :vm
                  :language :php)))
    (expect (typep result 'cl-cc/compile:compilation-result) :to-be-truthy)))


(it-sequential "php-e2e-match-and-relational-booleans"
  (expect (%php-run-capture "<?php $x=2; echo match($x){1=>'a',2=>'b'};") :to-equal "b")
  (expect (%php-run-capture "<?php $x=2; echo match($x){1,2=>'low',3=>'hi'};") :to-equal "low")
  (expect (%php-run-capture "<?php $x=3; echo match($x){1,2=>'low',3=>'hi'};") :to-equal "hi")
  (expect (%php-run-capture "<?php $x=5; echo match(true){$x>3=>'big',default=>'small'};") :to-equal "big")
  (expect (%php-run-capture "<?php $x=1; echo match(true){$x>3=>'big',default=>'small'};") :to-equal "small")
  (expect (%php-run-capture "<?php echo gettype(5>3);") :to-equal "boolean")
  (expect (%php-run-capture "<?php echo gettype(2<1);") :to-equal "boolean")
  (expect (%php-run-capture "<?php echo (5>3)===true ? 'T':'n';") :to-equal "T")
  (expect (%php-run-capture "<?php echo gettype(true);") :to-equal "boolean")
  (expect (%php-run-capture "<?php echo 'apple'<'banana' ? 'y':'n';") :to-equal "y")
  (expect (%php-run-capture "<?php $s=0; for($i=0;$i<4;$i++){$s+=$i;} echo $s;") :to-equal "6")
  (expect (%php-run-capture "<?php echo 1<=>2;") :to-equal "-1"))


(it-sequential "php-e2e-nullish-coalescing-assignment"
  (expect (%php-run-capture "<?php $a=null; $a ??= 'x'; echo $a;") :to-equal "x")
  (expect (%php-run-capture "<?php $a='keep'; $a ??= 'x'; echo $a;") :to-equal "keep")
  (expect (%php-run-capture "<?php $a=false; $a ??= 'x'; echo $a===false?'F':'o';") :to-equal "F")
  (expect (%php-run-capture "<?php $a=0; $a ??= 'x'; echo $a;") :to-equal "0")
  (expect (%php-run-capture "<?php class C{public $x;} $o=new C(); $o->x ??= 'y'; echo $o->x;") :to-equal "y")
  (expect (%php-run-capture "<?php class C{public $x=5;} $o=new C(); $o->x ??= 'y'; echo $o->x;") :to-equal "5")
  (expect (%php-run-capture "<?php $a=[]; $a['k'] ??= 'v'; echo $a['k'];") :to-equal "v")
  (expect (%php-run-capture "<?php $a=['k'=>3]; $a['k'] ??= 'v'; echo $a['k'];") :to-equal "3"))


(it-sequential "php-e2e-compound-assign-undefined-var"
  (expect (%php-run-capture "<?php $a += 3; echo $a;") :to-equal "3")
  (expect (%php-run-capture "<?php $a -= 3; echo $a;") :to-equal "-3")
  (expect (%php-run-capture "<?php $a *= 5; echo $a;") :to-equal "0")
  (expect (%php-run-capture "<?php $s .= 'hi'; echo $s;") :to-equal "hi")
  (expect (%php-run-capture "<?php $a ??= 'y'; echo $a;") :to-equal "y")
  (expect (%php-run-capture "<?php $a=5; $a += 3; echo $a;") :to-equal "8")
  (expect (%php-run-capture "<?php $a='7'; $a += '3'; echo $a;") :to-equal "10")
  (expect (%php-run-capture "<?php $a='10'; $a -= true; $a -= '1'; echo $a;") :to-equal "8")
  (expect (%php-run-capture "<?php $a='3'; $a *= '2'; echo $a;") :to-equal "6")
  (expect (%php-run-capture "<?php $s='x'; $s .= 'y'; echo $s;") :to-equal "xy"))


(it-sequential "php-e2e-arithmetic-operand-coercion"
  (expect (%php-run-capture "<?php echo null + 3;") :to-equal "3")
  (expect (%php-run-capture "<?php echo '5' + 3;") :to-equal "8")
  (expect (%php-run-capture "<?php echo '4' * '2';") :to-equal "8")
  (expect (%php-run-capture "<?php echo '1.5' + 1;") :to-equal "2.5")
  (expect (%php-run-capture "<?php echo true + 1;") :to-equal "2")
  (expect (%php-run-capture "<?php echo 2 - -3;") :to-equal "5")
  (expect (%php-run-capture "<?php echo +'7';") :to-equal "7")
  (expect (%php-run-capture "<?php echo -'7';") :to-equal "-7")
  (expect (%php-run-capture "<?php echo -null;") :to-equal "0")
  (expect (%php-run-capture "<?php echo -true;") :to-equal "-1")
  (expect (%php-run-capture "<?php echo 2 + 3;") :to-equal "5")
  (expect (%php-run-capture "<?php echo 2 + 3 * 4;") :to-equal "14")
  (expect (%php-run-capture "<?php $a=10; $a -= 3; echo $a;") :to-equal "7")
  (expect (%php-run-capture "<?php $s=0; for($i=1;$i<=4;$i++){$s+=$i;} echo $s;") :to-equal "10"))


(it-sequential "php-e2e-float-stringification"
  (expect (%php-run-capture "<?php echo 1.5 * 2;") :to-equal "3")
  (expect (%php-run-capture "<?php echo 1.5 + 0.25;") :to-equal "1.75")
  (expect (%php-run-capture "<?php echo 3.14;") :to-equal "3.14")
  (expect (%php-run-capture "<?php echo 6.0;") :to-equal "6")
  (expect (%php-run-capture "<?php echo 1000000.0;") :to-equal "1000000")
  (expect (%php-run-capture "<?php echo 10 / 4;") :to-equal "2.5")
  (expect (%php-run-capture "<?php echo 10 / 2;") :to-equal "5")
  (expect (%php-run-capture "<?php echo 'x=' . 1.5;") :to-equal "x=1.5")
  (expect (%php-run-capture "<?php $f=2.5; echo \"v=$f\";") :to-equal "v=2.5")
  (expect (%php-run-capture "<?php echo sqrt(2);") :to-equal "1.4142135623731")
  (expect (%php-run-capture "<?php echo sqrt(4);") :to-equal "2"))


(it-sequential "php-e2e-first-class-callable"
  (expect (%php-run-capture "<?php function sq($x){return $x*$x;} $f=sq(...); echo $f(5);") :to-equal "25")
  (expect (%php-run-capture "<?php function sub($a,$b){return $a-$b;} $f=sub(...); echo $f(10,3);") :to-equal "7")
  (expect (%php-run-capture "<?php function dbl($x){return $x*2;} $f=dbl(...); echo array_sum(array_map($f,[1,2,3]));") :to-equal "12")
  (expect (%php-run-capture "<?php $f=strlen(...); echo $f('hello');") :to-equal "5")
  (expect (%php-run-capture "<?php function sq($x){return $x*$x;} echo array_sum(array_map(sq(...),[1,2,3]));") :to-equal "14")
  (expect (%php-run-capture "<?php function sq($x){return $x*$x;} echo sq(5);") :to-equal "25")
  (expect (%php-run-capture "<?php function s(...$n){return array_sum($n);} $a=[1,2]; echo s(...$a);") :to-equal "3"))
