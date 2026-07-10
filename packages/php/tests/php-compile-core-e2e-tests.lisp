(in-package :cl-cc/test)

(in-suite cl-cc-php-e2e-suite)

(deftest php-e2e-postfix-incdec-on-places
  "Postfix ++/-- mutate an object property and an array element (regression: only a
plain $var was mutated), yielding the OLD value, including $this->n++ in a method."
  (assert-string= "2"  (%php-run-capture "<?php class C{ public $n=0; } $o=new C(); $o->n++; $o->n++; echo $o->n;"))
  (assert-string= "10" (%php-run-capture "<?php class C{ public $n=10; } $o=new C(); echo $o->n++;"))
  (assert-string= "4"  (%php-run-capture "<?php class C{ public $n=5; } $o=new C(); $o->n--; echo $o->n;"))
  ;; the canonical counter: $this->n++ inside a method
  (assert-string= "3"  (%php-run-capture "<?php class C{ public $n=0; function inc(){ $this->n++; } } $o=new C(); $o->inc(); $o->inc(); $o->inc(); echo $o->n;"))
  (assert-string= "2"  (%php-run-capture "<?php $a=[0]; $a[0]++; $a[0]++; echo $a[0];"))
  ;; postfix yields the old element value (single echo: ast-print adds a newline
  ;; per statement, so combine with '.')
  (assert-string= "5-6" (%php-run-capture "<?php $a=[5]; $old=$a[0]++; echo $old.'-'.$a[0];"))
  ;; PHP ++/-- use PHP numeric coercion for numeric strings, null and booleans.
  (assert-string= "7-8" (%php-run-capture "<?php $x='7'; $old=$x++; echo $old.'-'.$x;"))
  (assert-string= "1"   (%php-run-capture "<?php $x=null; ++$x; echo $x;"))
  (assert-string= "0"   (%php-run-capture "<?php $x=true; --$x; echo $x;")))


(deftest php-e2e-incdec-undefined-variable
  "Prefix and postfix ++/-- on an undefined variable introduce the variable
instead of reading an unbound Lisp symbol."
  (assert-string= "1"    (%php-run-capture "<?php $x++; echo $x;"))
  (assert-string= "1:1"  (%php-run-capture "<?php echo (++$x).':'.$x;"))
  (assert-string= "-1"   (%php-run-capture "<?php $x--; echo $x;"))
  (assert-string= "-1:-1" (%php-run-capture "<?php echo (--$x).':'.$x;")))


(deftest php-e2e-echo-no-trailing-newline-multi
  "Separate newline-less echos do NOT get a newline between them (regression:
echo lowered to ast-print -> vm-print '~A~%', appending a spurious newline after
every echo).  %php-run-capture trims only TRAILING newlines, so this is the
discriminator: old behaviour produced \"a\\nb\\nc\", now \"abc\"."
  (assert-string= "abc" (%php-run-capture "<?php echo 'a'; echo 'b'; echo 'c';")))


(deftest php-e2e-echo-no-trailing-newline-multiarg
  "Multi-argument echo concatenates its args with no separators or newline."
  (assert-string= "xyz" (%php-run-capture "<?php echo 'x', 'y', 'z';")))


(deftest php-e2e-echo-interior-newline-preserved
  "An explicit newline inside an echoed string is preserved (interior, not trailing).
NB: CL string literals have no \\n escape (\\ escapes the next char literally), so the
expected value is built with FORMAT ~% rather than written \"p\\nq\"."
  (assert-string= (format nil "p~%q") (%php-run-capture "<?php echo \"p\\n\"; echo 'q';")))


(deftest php-e2e-inline-html-verbatim
  "Inline HTML between ?> and <?php is emitted verbatim with no injected newline."
  (assert-string= "1hello2" (%php-run-capture "<?php echo 1; ?>hello<?php echo 2;")))


(deftest php-e2e-declare-directives
  "declare directives are compile-time wrappers here; statement and block bodies still run."
  (assert-string= "7" (%php-run-capture "<?php declare(strict_types=1); echo 7;"))
  (assert-string= "ABC" (%php-run-capture "<?php declare(ticks=1) { echo 'A'; echo 'B'; } echo 'C';"))
  (assert-string= "XY" (%php-run-capture "<?php declare(ticks=1) echo 'X'; echo 'Y';"))
  (assert-string= "ABC" (%php-run-capture "<?php declare(ticks=1): echo 'A'; echo 'B'; enddeclare; echo 'C';")))


(deftest php-e2e-output-buffering-builtins
  "Output buffering captures PHP output helpers until the active buffer is read or discarded."
  (assert-string= "ab"
                  (%php-run-capture "<?php ob_start(); echo 'a'; $s=ob_get_clean(); echo $s; echo 'b';"))
  (assert-string= "xy"
                  (%php-run-capture "<?php ob_start(); echo 'x'; $s=ob_get_contents(); ob_end_clean(); echo $s.'y';"))
  (assert-string= "ab"
                  (%php-run-capture "<?php ob_start(); echo 'a'; ob_start(); echo 'x'; ob_end_clean(); echo 'b'; echo ob_get_clean();"))
  (assert-string= "html!"
                  (%php-run-capture "<?php ob_start(); ?>html<?php $s=ob_get_clean(); echo $s.'!';")))


(deftest php-e2e-ini-get-set-and-error-reporting
  "ini_get/ini_set keep mutable configuration values and error_reporting()
shares the same error_reporting setting."
  (assert-string= "UTF-8" (%php-run-capture "<?php echo ini_get('default_charset');"))
  (assert-string= "UTF-8:Shift_JIS"
                  (%php-run-capture "<?php $old=ini_set('default_charset','Shift_JIS'); echo $old.':'.ini_get('default_charset'); ini_set('default_charset',$old);"))
  (assert-string= "32767:0:0"
                  (%php-run-capture "<?php error_reporting(32767); $old=error_reporting(0); echo $old.':'.error_reporting().':'.ini_get('error_reporting'); error_reporting($old);"))
  (assert-string= "32767:5:5"
                  (%php-run-capture "<?php error_reporting(32767); $old=ini_set('error_reporting','5'); echo $old.':'.error_reporting().':'.ini_get('error_reporting'); error_reporting($old);"))
  (assert-string= "false" (%php-run-capture "<?php echo ini_get('definitely_missing')===false?'false':'value';")))


(deftest php-e2e-error-and-exception-handler-registration
  "set_error_handler / restore_error_handler keep a PHP-visible handler stack,
route trigger_error through valid callbacks, and expose exception handler state."
  (assert-string= "H:512:boom"
                  (%php-run-capture "<?php function h($errno,$errstr,$file,$line){ echo 'H:'.$errno.':'.$errstr; return true; } set_error_handler('h', E_USER_WARNING); trigger_error('boom', E_USER_WARNING); restore_error_handler();"))
  (assert-string= "masked"
                  (%php-run-capture "<?php function only_notice($errno,$errstr){ echo 'handled'; return true; } set_error_handler('only_notice', E_USER_NOTICE); trigger_error('masked', E_USER_WARNING); restore_error_handler();"))
  (assert-string= "h1:two"
                  (%php-run-capture "<?php function h1($errno,$errstr){ echo 'one'; return true; } function h2($errno,$errstr){ echo 'two'; return true; } set_error_handler('h1'); $old=set_error_handler('h2'); echo $old.':'; trigger_error('x', E_USER_NOTICE); restore_error_handler(); restore_error_handler();"))
  (assert-string= "eh:ok"
                  (%php-run-capture "<?php function eh($e){ echo 'e'; } set_exception_handler('eh'); $old=set_exception_handler(function($e){ echo 'x'; }); echo $old.':ok'; restore_exception_handler(); restore_exception_handler();")))


(deftest php-e2e-error-suppression-operator
  "The @ operator evaluates the expression and restores error_reporting(), while
masking user-level errors from both the default reporter and custom handlers."
  (assert-string= "ok"
                  (%php-run-capture "<?php @trigger_error('hidden', E_USER_NOTICE); echo 'ok';"))
  (assert-string= "ok"
                  (%php-run-capture "<?php function h($errno,$errstr){ echo 'handler'; return true; } set_error_handler('h'); @trigger_error('hidden', E_USER_NOTICE); restore_error_handler(); echo 'ok';"))
  (assert-string= "1:32767"
                  (%php-run-capture "<?php error_reporting(E_ALL); $v=@trigger_error('hidden', E_USER_NOTICE); echo $v.':'.error_reporting();")))


(deftest php-e2e-date-default-timezone-and-ini
  "date_default_timezone_set/get share date.timezone and affect date(), while
gmdate() remains UTC."
  (assert-string= "UTC:Asia/Tokyo:Asia/Tokyo:09:00:00:00:0"
                  (%php-run-capture "<?php $old=date_default_timezone_get(); date_default_timezone_set('UTC'); $prev=date_default_timezone_get(); date_default_timezone_set('Asia/Tokyo'); echo $prev.':'.date_default_timezone_get().':'.ini_get('date.timezone').':'.date('H:i',0).':'.gmdate('H:i',0).':'.date('U',0); date_default_timezone_set($old);"))
  (assert-string= "Asia/Tokyo:UTC:UTC"
                  (%php-run-capture "<?php $old=date_default_timezone_get(); date_default_timezone_set('Asia/Tokyo'); $prev=ini_set('date.timezone','UTC'); echo $prev.':'.date_default_timezone_get().':'.ini_get('date.timezone'); date_default_timezone_set($old);"))
  (assert-string= "false:UTC"
                  (%php-run-capture "<?php $old=date_default_timezone_get(); date_default_timezone_set('UTC'); echo (date_default_timezone_set('No/Such_Zone')?'true':'false').':'.date_default_timezone_get(); date_default_timezone_set($old);"))
  (assert-string= "0:32400:1970-01-01 09:00"
                  (%php-run-capture "<?php $old=date_default_timezone_get(); date_default_timezone_set('Asia/Tokyo'); echo strtotime('1970-01-01 09:00:00').':'.mktime(18,0,0,1,1,1970).':'.date('Y-m-d H:i',0); date_default_timezone_set($old);")))


(deftest php-e2e-heredoc-nowdoc
  "Heredoc <<<EOT interpolates $vars and {$expr} like a double-quoted string;
nowdoc <<<'EOT' is literal.  Regression: the heredoc body was emitted as a raw
non-interpolated T-STRING (so $n printed literally), and the nowdoc label scan
swallowed the closing quote into the marker ('end marker EOT' not found').  Now
the heredoc body is routed through the shared interpolation lexer and the label
is scanned as an identifier."
  ;; heredoc interpolates a simple $var
  (assert-string= "Hi Bob"
                  (%php-run-capture (format nil "<?php $n=\"Bob\"; echo <<<EOT~%Hi $n~%EOT;~%")))
  ;; multiple vars and a {$expr} brace form
  (assert-string= "X-Y"
                  (%php-run-capture (format nil "<?php $a=\"X\";$b=\"Y\"; echo <<<EOT~%$a-$b~%EOT;~%")))
  (assert-string= "Hi Bob!"
                  (%php-run-capture (format nil "<?php $n=\"Bob\"; echo <<<EOT~%Hi {$n}!~%EOT;~%")))
  ;; multi-line body preserves interior newlines
  (assert-string= (format nil "line1~%val=Z")
                  (%php-run-capture (format nil "<?php $n=\"Z\"; echo <<<EOT~%line1~%val=$n~%EOT;~%")))
  ;; double-quoted heredoc label is still an (interpolating) heredoc
  (assert-string= "v=Q"
                  (%php-run-capture (format nil "<?php $n=\"Q\"; echo <<<\"EOT\"~%v=$n~%EOT;~%")))
  ;; nowdoc is literal — $n is NOT interpolated
  (assert-string= "Hi $n"
                  (%php-run-capture (format nil "<?php $n=\"Bob\"; echo <<<'EOT'~%Hi $n~%EOT;~%"))))


(deftest php-e2e-list-destructuring
  "list(...) = expr destructuring assignment works, mirroring the short [$a,$b]
form.  Regression: list($a,$b) lowered to a %php-list-bind call node that the
assignment parser did not recognize ('unsupported assignment target'); now it
parses to the same array-literal node as [$a,$b], flowing through
%php-lower-list-assign."
  (assert-string= "3"      (%php-run-capture "<?php list($a,$b)=[1,2]; echo $a+$b;"))
  (assert-string= "60"     (%php-run-capture "<?php list($a,$b,$c)=[10,20,30]; echo $a+$b+$c;"))
  ;; destructuring a function's array return
  (assert-string= "4-9"    (%php-run-capture "<?php function pair(){return [4,9];} list($x,$y)=pair(); echo $x.'-'.$y;"))
  (assert-string= "Bob:30" (%php-run-capture "<?php list($n,$a)=['Bob',30]; echo $n.':'.$a;"))
  ;; the short [$a,$b] form (and swap) keep working
  (assert-string= "1,2"    (%php-run-capture "<?php [$a,$b]=[1,2]; echo $a.','.$b;"))
  (assert-string= "2,1"    (%php-run-capture "<?php $a=1;$b=2; [$a,$b]=[$b,$a]; echo $a.','.$b;")))


(deftest php-e2e-list-destructuring-nested-keyed
  "Nested and keyed list destructuring.  %php-lower-list-assign previously bound
only flat top-level $var targets positionally, ignoring `key => $var' entries
and skipping nested [..] targets.  Now it recurses into nested targets and
accesses by key when a key is present."
  ;; nested
  (assert-string= "6"   (%php-run-capture "<?php [[$a,$b],$c]=[[1,2],3]; echo $a+$b+$c;"))
  (assert-string= "6"   (%php-run-capture "<?php list(list($a,$b),$c)=[[1,2],3]; echo $a+$b+$c;"))
  ;; keyed (order-independent access by key)
  (assert-string= "3"   (%php-run-capture "<?php ['x'=>$a,'y'=>$b]=['x'=>1,'y'=>2]; echo $a+$b;"))
  (assert-string= "1"   (%php-run-capture "<?php ['y'=>$a,'x'=>$b]=['x'=>1,'y'=>2]; echo $a-$b;")) ; a=2,b=1
  ;; nested + keyed combined
  (assert-string= "9,3" (%php-run-capture "<?php [['k'=>$a],$b]=[['k'=>9],3]; echo $a.','.$b;")))


(deftest php-e2e-complex-string-interpolation
  "Curly-brace interpolation {$expr} supports array access, nested access, and
method calls — not just a bare variable (regression: the lexer required } right
after {$var, so {$a[\"k\"]} raised a lex error)."
  (assert-string= "v=5" (%php-run-capture "<?php $a=['k'=>5]; echo \"v={$a['k']}\";"))
  (assert-string= "n=9" (%php-run-capture "<?php $a=['x'=>['y'=>9]]; echo \"n={$a['x']['y']}\";"))
  (assert-string= "m=42" (%php-run-capture "<?php class C{ function g(){ return 42; } } $o=new C(); echo \"m={$o->g()}\";"))
  (assert-string= "Hi Bob, age 30!"
                  (%php-run-capture "<?php $n='Bob'; $a=['age'=>30]; echo \"Hi $n, age {$a['age']}!\";"))
  ;; simple interpolation and plain strings unaffected
  (assert-string= "Hi Bob!" (%php-run-capture "<?php $n='Bob'; echo \"Hi $n!\";"))
  (assert-string= "Hi Bob!" (%php-run-capture "<?php $n='Bob'; echo \"Hi {$n}!\";")))


(deftest php-e2e-equality-operators
  "== / != / === / !== compile and evaluate (regression: these lowered to an
unknown op symbol, so any function using them was silently dropped)."
  (assert-string= "1"  (%php-run-capture "<?php function f($x){ return $x == 4; } echo f(4);"))
  (assert-string= ""   (%php-run-capture "<?php function f($x){ return $x == 4; } echo f(3);"))
  (assert-string= "1"  (%php-run-capture "<?php function f($x){ return $x != 4; } echo f(3);"))
  (assert-string= "1"  (%php-run-capture "<?php function f($x){ return $x % 2 == 0; } echo f(4);")))


(deftest php-e2e-equality-type-juggling
  "PHP == juggles types ('5' == 5 is true) while === is strict ('5' === 5 false)."
  (assert-string= "loose"
                  (%php-run-capture "<?php $a='5'; if ($a == 5) { echo 'loose'; }"))
  (assert-string= "strictfail"
                  (%php-run-capture "<?php $a='5'; if ($a === 5) { echo 'x'; } else { echo 'strictfail'; }")))


(deftest php-e2e-echo-boolean-php-semantics
  "echo converts with PHP string semantics: true -> '1', false -> '' (regression:
echo went to the generic VM printer and printed CL T/NIL). Single echo per case —
ast-print appends a newline per statement, so values are combined with '.' / ','."
  (assert-string= "1"  (%php-run-capture "<?php echo true;"))
  (assert-string= ""   (%php-run-capture "<?php echo false;"))
  (assert-string= "1X" (%php-run-capture "<?php echo true . 'X';"))
  (assert-string= "X"  (%php-run-capture "<?php echo false . 'X';"))
  (assert-string= "1"  (%php-run-capture "<?php echo (5 == 5);"))
  (assert-string= "123" (%php-run-capture "<?php echo 1,2,3;")))


(deftest php-e2e-logical-operators
  "&& / || / ! evaluate to PHP booleans (regression: these lowered to unknown op
symbols / an undefined cl-cc/php::! function, so any expression using them failed
to compile)."
  (assert-string= "T" (%php-run-capture "<?php echo (true && true) ? 'T':'F';"))
  (assert-string= "F" (%php-run-capture "<?php echo (true && false) ? 'T':'F';"))
  (assert-string= "T" (%php-run-capture "<?php echo (false || true) ? 'T':'F';"))
  (assert-string= "F" (%php-run-capture "<?php echo (false || false) ? 'T':'F';"))
  (assert-string= "T" (%php-run-capture "<?php echo !false ? 'T':'F';"))
  (assert-string= "F" (%php-run-capture "<?php echo !5 ? 'T':'F';"))
  (assert-string= "1" (%php-run-capture "<?php echo true && true;")))


(deftest php-e2e-logical-in-conditions
  "Logical operators compose in if conditions and short-circuit."
  (assert-string= "both" (%php-run-capture "<?php $a=1; $b=2; if ($a && $b) { echo 'both'; }"))
  (assert-string= "mid"  (%php-run-capture "<?php $x=5; if ($x > 0 && $x < 10) { echo 'mid'; }"))
  (assert-string= "ok"   (%php-run-capture "<?php $a=true; $b=false; if (!$b && $a) { echo 'ok'; }")))


(deftest php-e2e-logical-short-circuit
  "&& does not evaluate its right operand when the left is false (the divide-by-
zero in boom() must not run)."
  (assert-string= "F"
                  (%php-run-capture
                   "<?php function boom(){ return 1/0; } $x=false; echo ($x && boom()) ? 'T':'F';")))


(deftest php-e2e-default-arguments
  "Function parameters with `= default` bind the default when the call omits them,
and the passed value otherwise (regression: php-parse-param-list parsed then
discarded the default tokens, so omitted args were left unset / 0)."
  (assert-string= "7"  (%php-run-capture "<?php function g($x=7){ return $x; } echo g();"))
  (assert-string= "3"  (%php-run-capture "<?php function g($x=7){ return $x; } echo g(3);"))
  (assert-string= "hi" (%php-run-capture "<?php function g($s='hi'){ return $s; } echo g();"))
  (assert-string= "15" (%php-run-capture "<?php function f($a,$b=10){ return $a+$b; } echo f(5);"))
  (assert-string= "7"  (%php-run-capture "<?php function f($a,$b=10){ return $a+$b; } echo f(5,2);"))
  (assert-string= "6"  (%php-run-capture "<?php function f($a=1,$b=2,$c=3){ return $a+$b+$c; } echo f();"))
  (assert-string= "15" (%php-run-capture "<?php function f($a=1,$b=2,$c=3){ return $a+$b+$c; } echo f(10);")))


(deftest php-e2e-named-arguments
  "Named arguments are lowered to positional calls for user-defined PHP functions."
  (assert-string= "ABC"
                  (%php-run-capture
                   "<?php function join3($a,$b,$c){ return $a.$b.$c; } echo join3(c:'C', a:'A', b:'B');"))
  (assert-string= "A2C"
                  (%php-run-capture
                   "<?php function f($a='A',$b='B',$c='C'){ return $a.$b.$c; } echo f(c:'C', b:2);"))
  (assert-string= "XBC"
                  (%php-run-capture
                   "<?php function f($a='A',$b='B',$c='C'){ return $a.$b.$c; } echo f('X', c:'C');"))
  (assert-string= "ABC"
                  (%php-run-capture
                   "<?php function join3($a,$b,$c){ return $a.$b.$c; } echo join3(...['A'], b:'B', c:'C');"))
  (assert-string= "ABC"
                  (%php-run-capture
                   "<?php function join3($a,$b,$c){ return $a.$b.$c; } echo join3(...['A','B'], c:'C');"))
  (assert-string= "XBC"
                  (%php-run-capture
                   "<?php function f($a='A',$b='B',$c='C'){ return $a.$b.$c; } echo f(...['X'], c:'C');"))
  (assert-string= "ABC"
                  (%php-run-capture
                   "<?php class C{ function join3($a,$b,$c){ return $a.$b.$c; } } $c=new C(); echo $c->join3(c:'C', a:'A', b:'B');"))
  (assert-string= "ABC"
                  (%php-run-capture
                   "<?php class C{ function join3($a,$b,$c){ return $a.$b.$c; } } $c=new C(); echo $c?->join3(c:'C', a:'A', b:'B');"))
  (assert-string= "ABC"
                  (%php-run-capture
                   "<?php class C{ static function join3($a,$b,$c){ return $a.$b.$c; } } echo C::join3(c:'C', a:'A', b:'B');"))
  (assert-string= "ABC"
                  (%php-run-capture
                   "<?php class C{ function join3($a,$b,$c){ return $a.$b.$c; } } $c=new C(); echo $c->join3(...['A'], b:'B', c:'C');")))


(deftest php-e2e-default-arguments-all-callable-forms
  "Defaults work in closures, arrow functions, and methods, not just named functions."
  (assert-string= "9"  (%php-run-capture "<?php $f=function($x=9){ return $x; }; echo $f();"))
  (assert-string= "4"  (%php-run-capture "<?php $f=function($x=9){ return $x; }; echo $f(4);"))
  (assert-string= "16" (%php-run-capture "<?php $f=fn($x=8)=>$x*2; echo $f();"))
  (assert-string= "5"  (%php-run-capture "<?php class C{ function m($x=5){ return $x; } } $c=new C(); echo $c->m();"))
  (assert-string= "20" (%php-run-capture "<?php class C{ function m($x=5){ return $x; } } $c=new C(); echo $c->m(20);")))


(deftest php-e2e-variadic-parameters
  "...$args collects the trailing arguments into a PHP array (regression: the
variadic param received the first arg directly instead of a collected array)."
  (assert-string= "10" (%php-run-capture "<?php function s(...$n){ return array_sum($n); } echo s(1,2,3,4);"))
  (assert-string= "4"  (%php-run-capture "<?php function s(...$n){ return count($n); } echo s(1,2,3,4);"))
  (assert-string= "0"  (%php-run-capture "<?php function s(...$n){ return count($n); } echo s();"))
  (assert-string= "x:3" (%php-run-capture "<?php function f($a,...$rest){ return $a.':'.count($rest); } echo f('x',1,2,3);"))
  (assert-string= "abc" (%php-run-capture "<?php function j(...$xs){ $r=''; foreach($xs as $x){ $r.=$x; } return $r; } echo j('a','b','c');")))


(deftest php-e2e-variadic-all-callable-forms
  "Variadics work in closures and arrow functions too."
  (assert-string= "6"  (%php-run-capture "<?php $s=function(...$n){ return array_sum($n); }; echo $s(1,2,3);"))
  (assert-string= "15" (%php-run-capture "<?php $s=fn(...$n)=>array_sum($n); echo $s(5,5,5);")))


(deftest php-e2e-spread-call
  "f(...$args) expands an array into positional arguments (regression: spread
lowered to an unconsumed %php-spread marker). Lowers to apply over a runtime arg
list."
  (assert-string= "5"  (%php-run-capture "<?php function add($a,$b){ return $a+$b; } $args=[2,3]; echo add(...$args);"))
  (assert-string= "6"  (%php-run-capture "<?php function add3($a,$b,$c){ return $a+$b+$c; } $x=[1,2,3]; echo add3(...$x);"))
  (assert-string= "xyz" (%php-run-capture "<?php function f($a,$b,$c){ return $a.$b.$c; } $r=['y','z']; echo f('x',...$r);"))
  (assert-string= "3"  (%php-run-capture "<?php $a=[3,1,2]; echo max(...$a);"))
  (assert-string= "1"  (%php-run-capture "<?php $a=[3,1,2]; echo min(...$a);")))


(deftest php-e2e-named-args-after-dynamic-spread
  "Named arguments after a runtime-width spread are merged using function
parameter metadata."
  (assert-string= "abc"
                  (%php-run-capture
                   "<?php function j($a,$b,$c){ return $a.$b.$c; } $args=['a','b']; echo j(...$args, c:'c');"))
  (assert-string= "axc"
                  (%php-run-capture
                   "<?php function j($a,$b='x',$c='z'){ return $a.$b.$c; } $args=['a']; echo j(...$args, c:'c');")))


(deftest php-e2e-spread-composes-with-variadic-and-closures
  "Spread expands into a variadic collector, and works on dynamic closure calls."
  (assert-string= "60" (%php-run-capture "<?php function s(...$n){ return array_sum($n); } $a=[10,20,30]; echo s(...$a);"))
  (assert-string= "20" (%php-run-capture "<?php $f=function($a,$b){ return $a*$b; }; $args=[4,5]; echo $f(...$args);")))


(deftest php-e2e-for-loop-continue
  "continue in a for loop runs the increment before re-testing (regression: the
for->while lowering put the continue target before the increment, so continue
skipped $i++ and looped forever)."
  (assert-string= "6"  (%php-run-capture "<?php $s=0; for($i=0;$i<6;$i++){ if($i%2)continue; $s+=$i; } echo $s;"))
  (assert-string= "6"  (%php-run-capture "<?php $s=0; for($i=0;$i<10;$i++){ if($i==5)break; if($i%2)continue; $s+=$i; } echo $s;"))
  (assert-string= "6"  (%php-run-capture "<?php $s=0; for($i=0;$i<3;$i++){ for($j=0;$j<3;$j++){ if($j==1)continue; $s++; } } echo $s;")))


(deftest php-e2e-for-loop-basics
  "for loops without continue still iterate correctly (no regression from the
dedicated for lowering)."
  (assert-string= "10"  (%php-run-capture "<?php $s=0; for($i=1;$i<=4;$i++){ $s+=$i; } echo $s;"))
  (assert-string= "3"   (%php-run-capture "<?php $s=0; for($i=0;$i<10;$i++){ if($i==3)break; $s+=$i; } echo $s;"))
  (assert-string= "321" (%php-run-capture "<?php $s=''; for($i=3;$i>0;$i--){ $s.=$i; } echo $s;")))


(deftest php-parser-cli-compile-path-for-php-files
  "Characterization: native compile path should auto-detect .php files and compile PHP source end-to-end."
  (let* ((tmp-dir (uiop:temporary-directory))
         (input (merge-pathnames "cl-cc-php-compile-gap.php" tmp-dir))
         (output (merge-pathnames "cl-cc-php-compile-gap" tmp-dir)))
    (with-open-file (stream input :direction :output :if-exists :supersede)
      (write-line "<?php echo match($x) { 1 => 'one', default => 'other' };" stream))
    (let ((result (cl-cc::compile-file-to-native input :output-file output)))
      (assert-equal output result)
      (assert-true (probe-file output)))))


(deftest php-runtime-expression-operator-helpers
  "PHP operator helpers implement integer bitwise, shift, modulo, and spaceship behavior."
  (assert-= 3 (cl-cc/php:%php-modulo 7 4))
  (assert-= -1 (cl-cc/php:%php-modulo -7 3))
  (assert-= 8 (cl-cc/php:%php-shift-left 1 3))
  (assert-= -4 (cl-cc/php:%php-shift-right -8 1))
  (assert-= -1 (cl-cc/php:%php-spaceship 1 2))
  (assert-= 0 (cl-cc/php:%php-spaceship "2" 2))
  (assert-= 2 (cl-cc/php:%php-bitwise-and 6 3))
  (assert-= 7 (cl-cc/php:%php-bitwise-or 6 3))
  (assert-= 5 (cl-cc/php:%php-bitwise-xor 6 3))
  (assert-= -2 (cl-cc/php:%php-bitwise-not 1)))


(deftest php-compile-expression-operators
  "The PHP frontend compiles all expression operators added to the precedence chain."
  (let ((result (cl-cc:compile-string
                 "<?php $a = 2 ** 3 ** 2; $b = 7 % 4; $c = 1 << 3; $d = 8 >> 1; $e = 1 <=> 2; $f = 6 & 3; $g = 6 ^ 3; $h = 4 | 1; $i = ~1; $j = 1 + 2 . 3;"
                 :target :vm
                  :language :php)))
    (assert-true (typep result 'cl-cc/compile:compilation-result))))


(deftest php-e2e-match-and-relational-booleans
  "match(true){$cond=>…} works and relational operators return PHP booleans.
Two regressions: (1) match arms with multiple conditions (1,2=>…) built an
ast-binop :or that codegen could not emit, so the match produced nothing; now
they chain via nested ifs. (2) <,>,<=,>= lowered to the VM's integer compare
(1/0), so gettype(5>3) was \"integer\", (5>3)===true was false, and
match(true){$x>3=>…} never matched; now they go through %php-lt/gt/le/ge
(derived from %php-spaceship) and yield a real boolean."
  ;; match on a value (single and multiple conditions per arm)
  (assert-string= "b"   (%php-run-capture "<?php $x=2; echo match($x){1=>'a',2=>'b'};"))
  (assert-string= "low" (%php-run-capture "<?php $x=2; echo match($x){1,2=>'low',3=>'hi'};"))
  (assert-string= "hi"  (%php-run-capture "<?php $x=3; echo match($x){1,2=>'low',3=>'hi'};"))
  ;; match(true) with comparison conditions
  (assert-string= "big"   (%php-run-capture "<?php $x=5; echo match(true){$x>3=>'big',default=>'small'};"))
  (assert-string= "small" (%php-run-capture "<?php $x=1; echo match(true){$x>3=>'big',default=>'small'};"))
  ;; relational operators yield PHP booleans
  (assert-string= "boolean" (%php-run-capture "<?php echo gettype(5>3);"))
  (assert-string= "boolean" (%php-run-capture "<?php echo gettype(2<1);"))
  (assert-string= "T"       (%php-run-capture "<?php echo (5>3)===true ? 'T':'n';"))
  (assert-string= "boolean" (%php-run-capture "<?php echo gettype(true);"))
  ;; string comparison and loop conditions still work
  (assert-string= "y"  (%php-run-capture "<?php echo 'apple'<'banana' ? 'y':'n';"))
  (assert-string= "6"  (%php-run-capture "<?php $s=0; for($i=0;$i<4;$i++){$s+=$i;} echo $s;"))
  ;; spaceship still returns -1/0/1
  (assert-string= "-1" (%php-run-capture "<?php echo 1<=>2;")))


(deftest php-e2e-nullish-coalescing-assignment
  "$x ??= v assigns only when $x is PHP null, for variables, object properties
and array elements.  Regression: %php-nullish-cond built an (ast-binop :op 'or)
that codegen could not emit, so EVERY ??= failed to compile and dropped the whole
program; it also wrongly treated false/0 as nullish."
  ;; variable: assigns when null, keeps an existing value
  (assert-string= "x"    (%php-run-capture "<?php $a=null; $a ??= 'x'; echo $a;"))
  (assert-string= "keep" (%php-run-capture "<?php $a='keep'; $a ??= 'x'; echo $a;"))
  ;; false and 0 are NOT nullish — ??= must not overwrite them
  (assert-string= "F"    (%php-run-capture "<?php $a=false; $a ??= 'x'; echo $a===false?'F':'o';"))
  (assert-string= "0"    (%php-run-capture "<?php $a=0; $a ??= 'x'; echo $a;"))
  ;; object property
  (assert-string= "y"    (%php-run-capture "<?php class C{public $x;} $o=new C(); $o->x ??= 'y'; echo $o->x;"))
  (assert-string= "5"    (%php-run-capture "<?php class C{public $x=5;} $o=new C(); $o->x ??= 'y'; echo $o->x;"))
  ;; array element
  (assert-string= "v"    (%php-run-capture "<?php $a=[]; $a['k'] ??= 'v'; echo $a['k'];"))
  (assert-string= "3"    (%php-run-capture "<?php $a=['k'=>3]; $a['k'] ??= 'v'; echo $a['k'];")))


(deftest php-e2e-compound-assign-undefined-var
  "A compound assignment on an undefined variable treats the missing left
operand as null (0 for arithmetic/bitwise, '' for concat, RHS for ??=) and
INTRODUCES the variable.  Regression: it read the unbound variable into a temp
and dropped the whole program."
  (assert-string= "3"  (%php-run-capture "<?php $a += 3; echo $a;"))
  (assert-string= "-3" (%php-run-capture "<?php $a -= 3; echo $a;"))
  (assert-string= "0"  (%php-run-capture "<?php $a *= 5; echo $a;"))
  (assert-string= "hi" (%php-run-capture "<?php $s .= 'hi'; echo $s;"))
  (assert-string= "y"  (%php-run-capture "<?php $a ??= 'y'; echo $a;"))
  ;; the known-variable path is unaffected
  (assert-string= "8"  (%php-run-capture "<?php $a=5; $a += 3; echo $a;"))
  (assert-string= "10" (%php-run-capture "<?php $a='7'; $a += '3'; echo $a;"))
  (assert-string= "8"  (%php-run-capture "<?php $a='10'; $a -= true; $a -= '1'; echo $a;"))
  (assert-string= "6"  (%php-run-capture "<?php $a='3'; $a *= '2'; echo $a;"))
  (assert-string= "xy" (%php-run-capture "<?php $s='x'; $s .= 'y'; echo $s;")))


(deftest php-e2e-arithmetic-operand-coercion
  "PHP +, -, * coerce non-number operands (null->0, true->1, numeric string->
number) instead of erroring.  Regression: they lowered to raw CL arithmetic, so
null + 3, '5' + 3, true + 1 all signalled `not of type NUMBER'."
  (assert-string= "3"   (%php-run-capture "<?php echo null + 3;"))
  (assert-string= "8"   (%php-run-capture "<?php echo '5' + 3;"))
  (assert-string= "8"   (%php-run-capture "<?php echo '4' * '2';"))
  (assert-string= "2.5" (%php-run-capture "<?php echo '1.5' + 1;"))
  (assert-string= "2"   (%php-run-capture "<?php echo true + 1;"))
  (assert-string= "5"   (%php-run-capture "<?php echo 2 - -3;"))
  (assert-string= "7"   (%php-run-capture "<?php echo +'7';"))
  (assert-string= "-7"  (%php-run-capture "<?php echo -'7';"))
  (assert-string= "0"   (%php-run-capture "<?php echo -null;"))
  (assert-string= "-1"  (%php-run-capture "<?php echo -true;"))
  ;; pure-number arithmetic, precedence, compound and loops are unaffected
  (assert-string= "5"   (%php-run-capture "<?php echo 2 + 3;"))
  (assert-string= "14"  (%php-run-capture "<?php echo 2 + 3 * 4;"))
  (assert-string= "7"   (%php-run-capture "<?php $a=10; $a -= 3; echo $a;"))
  (assert-string= "10"  (%php-run-capture "<?php $s=0; for($i=1;$i<=4;$i++){$s+=$i;} echo $s;")))


(deftest php-e2e-float-stringification
  "PHP echoes/interpolates floats correctly: whole-valued floats as integers,
others with trailing zeros trimmed and no CL exponent marker; float literals
keep double precision; / yields a float (or an int when evenly divisible).
Regression: %php-stringify used princ-to-string (1.5 -> \"1.5d0\", 5/2 -> \"5/2\"),
literals were read as single-floats (3.14 -> 3.1400001…), and / returned a
rational."
  (assert-string= "3"       (%php-run-capture "<?php echo 1.5 * 2;"))
  (assert-string= "1.75"    (%php-run-capture "<?php echo 1.5 + 0.25;"))
  (assert-string= "3.14"    (%php-run-capture "<?php echo 3.14;"))
  (assert-string= "6"       (%php-run-capture "<?php echo 6.0;"))
  (assert-string= "1000000" (%php-run-capture "<?php echo 1000000.0;"))
  (assert-string= "2.5"     (%php-run-capture "<?php echo 10 / 4;"))
  (assert-string= "5"       (%php-run-capture "<?php echo 10 / 2;"))
  (assert-string= "x=1.5"   (%php-run-capture "<?php echo 'x=' . 1.5;"))
  (assert-string= "v=2.5"   (%php-run-capture "<?php $f=2.5; echo \"v=$f\";"))
  ;; sqrt returns a double (PHP 14-digit precision), whole roots print as int
  (assert-string= "1.4142135623731" (%php-run-capture "<?php echo sqrt(2);"))
  (assert-string= "2"       (%php-run-capture "<?php echo sqrt(4);")))


(deftest php-e2e-first-class-callable
  "f(...) creates a first-class callable (PHP 8.1) — a forwarding closure
fn(...$a) => f(...$a) — for user functions and builtins.  Regression: f(...)
parsed the ... as a %php-first-class-callable marker ARGUMENT, so f(...) called
f with the marker ('Undefined function: %PHP-FIRST-CLASS-CALLABLE')."
  (assert-string= "25" (%php-run-capture "<?php function sq($x){return $x*$x;} $f=sq(...); echo $f(5);"))
  ;; forwards multiple arguments
  (assert-string= "7"  (%php-run-capture "<?php function sub($a,$b){return $a-$b;} $f=sub(...); echo $f(10,3);"))
  ;; usable as a callable in higher-order functions
  (assert-string= "12" (%php-run-capture "<?php function dbl($x){return $x*2;} $f=dbl(...); echo array_sum(array_map($f,[1,2,3]));"))
  ;; works for builtins
  (assert-string= "5"  (%php-run-capture "<?php $f=strlen(...); echo $f('hello');"))
  ;; inline as an argument
  (assert-string= "14" (%php-run-capture "<?php function sq($x){return $x*$x;} echo array_sum(array_map(sq(...),[1,2,3]));"))
  ;; regression guards: ordinary calls and call-spread unaffected
  (assert-string= "25" (%php-run-capture "<?php function sq($x){return $x*$x;} echo sq(5);"))
  (assert-string= "3"  (%php-run-capture "<?php function s(...$n){return array_sum($n);} $a=[1,2]; echo s(...$a);")))
