(in-package :cl-cc/test)


(it-sequential "php-runtime-yield-helper-preserves-value"
  (expect (cl-cc/php:%php-yield 42) :to-equal '(:yield 42))
  (expect (cl-cc/php:%php-yield-from '(1 2)) :to-equal '(:yield-from (1 2))))


(it-sequential "php-runtime-exception-payload-matches-class"
  (let ((payload (cl-cc/php:%php-make-exception 'ex :value)))
    (expect (cl-cc/php:%php-exception-object-p payload) :to-be-truthy)
    (expect (cl-cc/php:%php-exception-value payload) :to-be :value)
    (expect (cl-cc/php:%php-exception-matches-p payload 'ex) :to-be-truthy)
    (expect (cl-cc/php:%php-exception-matches-p payload '(other ex)) :to-be-truthy)
    (expect (cl-cc/php:%php-exception-matches-p payload 'other) :to-be-falsy)))


(it-sequential "php-runtime-concat-stringifies-values"
  (expect (cl-cc/php:%php-concat "Hello " 42) :to-equal "Hello 42")
  (expect (cl-cc/php:%php-concat "x" t) :to-equal "x1")
  (expect (cl-cc/php:%php-concat "x" cl-cc/php:+php-null+) :to-equal "x"))


(it-sequential "php-e2e-var-dump-and-export"
  (expect (%php-run-capture "<?php var_dump(42);") :to-equal "int(42)")
  (expect (%php-run-capture "<?php var_dump('hi');") :to-equal "string(2) \"hi\"")
  (expect (%php-run-capture "<?php var_dump(true);") :to-equal "bool(true)")
  (expect (%php-run-capture "<?php var_dump(1.5);") :to-equal "float(1.5)")
  (expect (%php-run-capture "<?php var_dump(null);") :to-equal "NULL")
  (expect (%php-run-capture "<?php var_dump([1,2]);") :to-equal (format nil "array(2) {~%  [0]=>~%  int(1)~%  [1]=>~%  int(2)~%}"))
  (expect (%php-run-capture "<?php var_export(42);") :to-equal "42")
  (expect (%php-run-capture "<?php var_export('hi');") :to-equal "'hi'")
  (expect (%php-run-capture "<?php var_export(true);") :to-equal "true")
  (expect (%php-run-capture "<?php var_export([1,2]);") :to-equal (format nil "array (~%  0 => 1,~%  1 => 2,~%)"))
  (expect (%php-run-capture "<?php echo var_export(7, true);") :to-equal "7"))


(it-sequential "php-e2e-math-non-cl-named-builtins"
  (expect (%php-run-capture "<?php echo fmod(7,3);") :to-equal "1")
  (expect (%php-run-capture "<?php echo fmod(-7,3);") :to-equal "-1")
  (expect (%php-run-capture "<?php echo round(atan2(1,1),4);") :to-equal "0.7854")
  (expect (%php-run-capture "<?php echo log10(1000);") :to-equal "3")
  (expect (%php-run-capture "<?php echo log2(8);") :to-equal "3")
  (expect (%php-run-capture "<?php echo hypot(3,4);") :to-equal "5")
  (expect (%php-run-capture "<?php echo round(deg2rad(180),5);") :to-equal "3.14159")
  (expect (%php-run-capture "<?php echo round(rad2deg(3.141592653589793),2);") :to-equal "180")
  (expect (%php-run-capture "<?php echo base_convert('ff',16,2);") :to-equal "11111111")
  (expect (%php-run-capture "<?php echo base_convert('255',10,16);") :to-equal "ff")
  (expect (%php-run-capture "<?php echo is_finite(1.5)?'y':'n';") :to-equal "y")
  (expect (%php-run-capture "<?php echo is_infinite(fdiv(1,0))?'y':'n';") :to-equal "y"))


(it-sequential "php-e2e-serialize-unserialize"
  (expect (%php-run-capture "<?php echo serialize(42);") :to-equal "i:42;")
  (expect (%php-run-capture "<?php echo serialize('hello');") :to-equal "s:5:\"hello\";")
  (expect (%php-run-capture "<?php echo serialize(true);") :to-equal "b:1;")
  (expect (%php-run-capture "<?php echo serialize(false);") :to-equal "b:0;")
  (expect (%php-run-capture "<?php echo serialize(null);") :to-equal "N;")
  (expect (%php-run-capture "<?php echo serialize(3.14);") :to-equal "d:3.14;")
  (expect (%php-run-capture "<?php echo serialize([1,2,3]);") :to-equal "a:3:{i:0;i:1;i:1;i:2;i:2;i:3;}")
  (expect (%php-run-capture "<?php echo serialize(['a'=>1,'b'=>2]);") :to-equal "a:2:{s:1:\"a\";i:1;s:1:\"b\";i:2;}")
  (expect (%php-run-capture "<?php class C { public $x = 7; } echo serialize(new C());") :to-equal "O:1:\"C\":1:{s:1:\"x\";i:7;}")
  (expect (%php-run-capture "<?php class C { public $x = 7; public $y = 9; function __sleep(){ return ['y','x']; } } echo serialize(new C());") :to-equal "O:1:\"C\":2:{s:1:\"y\";i:9;s:1:\"x\";i:7;}")
  (expect (%php-run-capture "<?php class C { public $x = 7; public $y = 9; function __sleep(){ return ['y']; } function __wakeup(){ $this->x = 6; } } $u = unserialize(serialize(new C())); echo $u->x + $u->y;") :to-equal "15")
  (expect (%php-run-capture "<?php class C { public $x = 7; public $y = 9; function __serialize(){ return ['y'=>$this->y, 'x'=>$this->x]; } } echo serialize(new C());") :to-equal "O:1:\"C\":2:{s:1:\"y\";i:9;s:1:\"x\";i:7;}")
  (expect (%php-run-capture "<?php class C { public $x = 7; public $y = 9; function __serialize(){ return ['y'=>$this->y, 'x'=>$this->x]; } function __unserialize($data){ $this->y = $data['y']; $this->x = $data['x'] - 1; } } $u = unserialize(serialize(new C())); echo $u->x + $u->y;") :to-equal "15")
  (expect (%php-run-capture "<?php echo unserialize(serialize(42))+8;") :to-equal "50")
  (expect (%php-run-capture "<?php $x=unserialize(serialize([1,2,['k'=>'v']])); echo $x[2]['k'];") :to-equal "v")
  (expect (%php-run-capture "<?php class C { public $x = 7; } $u = unserialize(serialize(new C())); echo $u->x;") :to-equal "7")
  (expect (%php-run-capture "<?php echo unserialize('b:1;')?'T':'F';") :to-equal "T")
  (expect (%php-run-capture "<?php echo unserialize('garbage')?'T':'F';") :to-equal "F"))


(it-sequential "php-e2e-string-escape-preservation"
  (expect (%php-run-capture "<?php echo \"a\\.b\";") :to-equal "a\\.b")
  (expect (%php-run-capture "<?php echo \"\\d\\w\";") :to-equal "\\d\\w")
  (expect (%php-run-capture "<?php echo strlen(\"a\\nb\");") :to-equal "3")
  (expect (%php-run-capture "<?php echo \"\\x41\";") :to-equal "A")
  (expect (%php-run-capture "<?php echo \"\\u{48}\";") :to-equal "H")
  (expect (%php-run-capture "<?php echo strlen(\"\\t\");") :to-equal "1"))    ; tab


(it-sequential "php-e2e-md5-sha1-builtins"
  (expect (%php-run-capture "<?php echo md5('abc');") :to-equal "900150983cd24fb0d6963f7d28e17f72")
  (expect (%php-run-capture "<?php echo md5('');") :to-equal "d41d8cd98f00b204e9800998ecf8427e")
  (expect (%php-run-capture "<?php echo bin2hex(md5('abc', true));") :to-equal "900150983cd24fb0d6963f7d28e17f72")
  (expect (%php-run-capture "<?php echo sha1('abc');") :to-equal "a9993e364706816aba3e25717850c26c9cd0d89d")
  (expect (%php-run-capture "<?php echo sha1('');") :to-equal "da39a3ee5e6b4b0d3255bfef95601890afd80709")
  (expect (%php-run-capture "<?php echo bin2hex(sha1('abc', true));") :to-equal "a9993e364706816aba3e25717850c26c9cd0d89d"))


(it-sequential "php-e2e-crc32-builtin"
  (expect (%php-run-capture "<?php echo crc32('');") :to-equal "0")
  (expect (%php-run-capture "<?php echo crc32('abc');") :to-equal "891568578")
  (expect (%php-run-capture "<?php echo crc32('foo');") :to-equal "2356372769"))


(it-sequential "php-e2e-preg-replace-callback"
  (expect (%php-run-capture "<?php echo preg_replace_callback('/\\d/', fn($m)=>$m[0]*2, 'a1b2');") :to-equal "a2b4")
  (expect (%php-run-capture "<?php echo preg_replace_callback('/[a-z]+/', fn($m)=>strtoupper($m[0]), 'hi there');") :to-equal "HI THERE")
  (expect (%php-run-capture "<?php echo preg_replace_callback('/\\d/', fn($m)=>'X', '1234', 2);") :to-equal "XX34")
  (expect (%php-run-capture "<?php echo preg_replace_callback(\"/\\d/\", fn($m)=>$m[0]*2, 'a1b2');") :to-equal "a2b4")
  (expect (%php-run-capture "<?php echo preg_replace_callback('/(\\w+)=(\\d+)/', fn($m)=>$m[1].':'.$m[2], 'x=12 y=34');") :to-equal "x:12 y:34")
  (expect (%php-run-capture "<?php echo preg_replace_callback('/((\\d)(\\d))/', fn($m)=>$m[2].'-'.$m[3], '42');") :to-equal "4-2")
  (expect (%php-run-capture "<?php echo preg_replace_callback_array(['/\\d/'=>fn($m)=>'N','/[a-z]/'=>fn($m)=>'L'], 'a1b2');") :to-equal "LNLN")
  (expect (%php-run-capture "<?php echo preg_replace_callback_array(['/(\\w)(\\d)/'=>fn($m)=>$m[2].$m[1]], 'a1 b2');") :to-equal "1a 2b"))


(it-sequential "php-e2e-preg-capture-groups"
  (expect (%php-run-capture "<?php echo preg_replace('/(\\w)(\\w)/', '$2$1', 'abcd');") :to-equal "badc")
  (expect (%php-run-capture "<?php echo preg_replace('/(\\d+)-(\\d+)/', '$2/$1', '12-34');") :to-equal "34/12")
  (expect (%php-run-capture "<?php echo preg_replace('/\\d+/', '[$0]', 'a12b');") :to-equal "a[12]b")
  (expect (%php-run-capture "<?php echo preg_replace('/(\\w+)/', '${1}!', 'hi');") :to-equal "hi!")
  (expect (%php-run-capture "<?php echo preg_replace('/((\\d)(\\d))/', '$2-$3', '42');") :to-equal "4-2")
  (expect (%php-run-capture "<?php echo preg_match_all('/\\d/', '1a2b3');") :to-equal "3")
  (expect (%php-run-capture "<?php echo preg_match_all('/\\w+/', 'foo bar baz');") :to-equal "3")
  (expect (%php-run-capture "<?php echo preg_match_all('/\\d/', 'abc');") :to-equal "0"))


(it-sequential "php-e2e-preg-match-out-param"
  (expect (%php-run-capture "<?php preg_match('/(\\d+)-(\\d+)/', '12-34', $m); echo $m[1].'|'.$m[2];") :to-equal "12|34")
  (expect (%php-run-capture "<?php $r=preg_match('/(\\d+)/', 'abc123', $m); echo $r.':'.$m[0].':'.$m[1];") :to-equal "1:123:123")
  (expect (%php-run-capture "<?php preg_match('/(\\w+)@(\\w+)/', 'bob@host', $m); echo $m[1].' at '.$m[2];") :to-equal "bob at host")
  (expect (%php-run-capture "<?php echo preg_match('/\\d/', 'abc', $m);") :to-equal "0")
  (expect (%php-run-capture "<?php preg_match_all('/(\\d)/', '1a2b3', $m); echo implode(',', $m[1]);") :to-equal "1,2,3")
  (expect (%php-run-capture "<?php preg_match_all('/\\d+/', 'a12b34', $m); echo implode(',', $m[0]);") :to-equal "12,34")
  (expect (%php-run-capture "<?php $r=preg_match('/\\d/', 'a1b2c3', $m, 0, 3); echo $r.':'.$m[0];") :to-equal "1:2")
  (expect (%php-run-capture "<?php preg_match_all('/(\\d)/', 'a1b2c3', $m, 0, 3); echo implode(',', $m[1]);") :to-equal "2,3"))


(it-sequential "php-e2e-ucwords-delimiters"
  (expect (%php-run-capture "<?php echo ucwords('hello world');") :to-equal "Hello World")
  (expect (%php-run-capture "<?php echo ucwords('world order roar');") :to-equal "World Order Roar")
  (expect (%php-run-capture "<?php echo ucwords('fluffy vivid');") :to-equal "Fluffy Vivid")
  (expect (%php-run-capture "<?php echo ucwords('the QUICK brown');") :to-equal "The QUICK Brown")
  (expect (%php-run-capture "<?php echo ucwords('hello-world', '-');") :to-equal "Hello-World"))


(it-sequential "php-e2e-wordwrap-cut"
  (expect (%php-run-capture "<?php echo wordwrap('aaaaaa',3,'|',true);") :to-equal "aaa|aaa")
  (expect (%php-run-capture "<?php echo wordwrap('a verylongword b',4,'|',true);") :to-equal "a|very|long|word|b")
  (expect (%php-run-capture "<?php echo wordwrap('A very long woooooooord.',8,'|',true);") :to-equal "A very|long|wooooooo|ord.")
  (expect (%php-run-capture "<?php echo wordwrap('aaaaaa',3,'|',false);") :to-equal "aaaaaa")
  (expect (%php-run-capture "<?php echo wordwrap('aaa bbb ccc',5,'|');") :to-equal "aaa|bbb|ccc")
  (expect (%php-run-capture "<?php echo wordwrap('The quick brown fox',10,'|');") :to-equal "The quick|brown fox"))


(it-sequential "php-e2e-json-encode-pretty"
  (expect (%php-run-capture "<?php echo json_encode(['a'=>1,'b'=>[2,3]]);") :to-equal "{\"a\":1,\"b\":[2,3]}")
  (expect (%php-run-capture "<?php echo json_encode(['a'=>1,'b'=>[2,3]],JSON_PRETTY_PRINT);") :to-equal (format nil "{~%    \"a\": 1,~%    \"b\": [~%        2,~%        3~%    ]~%}"))
  (expect (%php-run-capture "<?php echo json_encode([1,2,3],JSON_PRETTY_PRINT);") :to-equal (format nil "[~%    1,~%    2,~%    3~%]"))
  (expect (%php-run-capture "<?php echo json_encode(['x'=>[],'y'=>1],JSON_PRETTY_PRINT);") :to-equal (format nil "{~%    \"x\": [],~%    \"y\": 1~%}")))


(it-sequential "php-e2e-json-decode"
  (expect (%php-run-capture "<?php $d=json_decode('{\"a\":1,\"b\":2}',true); echo $d['a']+$d['b'];") :to-equal "3")
  (expect (%php-run-capture "<?php $d=json_decode('{\"name\":\"Bob\",\"age\":30}',true); echo $d['name'].'-'.$d['age'];") :to-equal "Bob-30")
  (expect (%php-run-capture "<?php $d=json_decode('{\"x\":{\"y\":[1,2,3]}}',true); echo $d['x']['y'][2];") :to-equal "3")
  (expect (%php-run-capture "<?php $d=json_decode('[\"a\",\"bb\",\"ccc\"]'); echo $d[1].strlen($d[2]);") :to-equal "bb3")
  (expect (%php-run-capture "<?php echo json_decode('true')?'y':'n';") :to-equal "y")
  (expect (%php-run-capture "<?php echo json_decode('not json')===null?'null':'x';") :to-equal "null")
  (expect (%php-run-capture "<?php $o=['user'=>'alice','roles'=>['admin','editor'],'active'=>true]; $r=json_decode(json_encode($o),true); echo $r['user'].':'.$r['roles'][1].':'.($r['active']?'y':'n');") :to-equal "alice:editor:y"))


(it-sequential "php-e2e-number-format-rounding"
  (expect (%php-run-capture "<?php echo number_format(1234.5);") :to-equal "1,235")
  (expect (%php-run-capture "<?php echo number_format(2.5);") :to-equal "3")
  (expect (%php-run-capture "<?php echo number_format(0.5);") :to-equal "1")
  (expect (%php-run-capture "<?php echo number_format(1234.4);") :to-equal "1,234")
  (expect (%php-run-capture "<?php echo number_format(-1234.5);") :to-equal "-1,235")
  (expect (%php-run-capture "<?php echo number_format(3.14159,2);") :to-equal "3.14")
  (expect (%php-run-capture "<?php echo number_format(1234.567,2);") :to-equal "1,234.57")
  (expect (%php-run-capture "<?php echo number_format(1234.5,2,',','.');") :to-equal "1.234,50"))


(it-sequential "php-e2e-round-half-away"
  (expect (%php-run-capture "<?php echo round(2.5);") :to-equal "3")
  (expect (%php-run-capture "<?php echo round(3.5);") :to-equal "4")
  (expect (%php-run-capture "<?php echo round(-2.5);") :to-equal "-3")
  (expect (%php-run-capture "<?php echo round(0.5);") :to-equal "1")
  (expect (%php-run-capture "<?php echo round(2.4);") :to-equal "2")
  (expect (%php-run-capture "<?php echo round(-2.4);") :to-equal "-2")
  (expect (%php-run-capture "<?php echo round(3.14159,2);") :to-equal "3.14")
  (expect (%php-run-capture "<?php echo round(1.95583,2);") :to-equal "1.96")
  (expect (%php-run-capture "<?php echo round(1241757,-3);") :to-equal "1242000"))


(it-sequential "php-e2e-sprintf-flags"
  (expect (%php-run-capture "<?php echo sprintf('%e',12345.678);") :to-equal "1.234568e+4")
  (expect (%php-run-capture "<?php echo sprintf('%E',0.00012);") :to-equal "1.200000E-4")
  (expect (%php-run-capture "<?php echo sprintf('%+d %+d',5,-5);") :to-equal "+5 -5")
  (expect (%php-run-capture "<?php echo sprintf('%+05d',42);") :to-equal "+0042")
  (expect (%php-run-capture "<?php echo sprintf('%05d',-42);") :to-equal "-0042")
  (expect (%php-run-capture "<?php echo sprintf(\"%'*10d\",42);") :to-equal "********42")
  (expect (%php-run-capture "<?php echo sprintf(\"%'-10s\",'hi');") :to-equal "--------hi")
  (expect (%php-run-capture "<?php echo sprintf('%05d',42);") :to-equal "00042")
  (expect (%php-run-capture "<?php echo sprintf('[%-5s]','ab');") :to-equal "[ab   ]")
  (expect (%php-run-capture "<?php echo sprintf('%.2f',3.14159);") :to-equal "3.14")
  (expect (%php-run-capture "<?php echo sprintf('%x %X %o',255,255,8);") :to-equal "ff FF 10"))


(it-sequential "php-e2e-base-conversions"
  (expect (%php-run-capture "<?php echo dechex(255);") :to-equal "ff")
  (expect (%php-run-capture "<?php echo hexdec('ff');") :to-equal "255")
  (expect (%php-run-capture "<?php echo decbin(10);") :to-equal "1010")
  (expect (%php-run-capture "<?php echo bindec('1010');") :to-equal "10")
  (expect (%php-run-capture "<?php echo decoct(64);") :to-equal "100")
  (expect (%php-run-capture "<?php echo octdec('100');") :to-equal "64")
  (expect (%php-run-capture "<?php echo dechex(-1);") :to-equal "ffffffffffffffff")
  (expect (%php-run-capture "<?php echo hexdec(dechex(48879));") :to-equal "48879"))


(it-sequential "php-e2e-max-min"
  (expect (%php-run-capture "<?php echo max([3,1,2]);") :to-equal "3")
  (expect (%php-run-capture "<?php echo min([3,1,2]);") :to-equal "1")
  (expect (%php-run-capture "<?php echo max(1,5,3);") :to-equal "5")
  (expect (%php-run-capture "<?php echo max(1,'10',5);") :to-equal "10")
  (expect (%php-run-capture "<?php echo min(2.5,2,3);") :to-equal "2")
  (expect (%php-run-capture "<?php echo max(42);") :to-equal "42"))


(it-sequential "php-e2e-symbol-registered-builtins"
  (cl-cc/php::%php-register-all-builtins)
  (expect (cl-cc/php::%php-lookup-builtin-symbol "echo") :to-be 'cl-cc/php::%php-echo)
  (expect (cl-cc/php::%php-lookup-builtin-symbol "print") :to-be 'cl-cc/php::%php-print)
  (expect (with-output-to-string (out)
                    (let ((*standard-output* out))
                      (funcall (cl-cc/php::%php-lookup-builtin "echo") "a" "b"))) :to-equal "ab")
  (expect (with-output-to-string (out)
                    (let ((*standard-output* out))
                      (princ (funcall (cl-cc/php::%php-lookup-builtin "print") "c")))) :to-equal "c1")
  (expect (%php-run-capture "<?php function g(){yield 1;yield 2;} echo count(iterator_to_array(g()));") :to-equal "2")
  (expect (%php-run-capture "<?php function g(){yield 1;yield 2;yield 3;} echo iterator_count(g());") :to-equal "3")
  (expect (%php-run-capture "<?php echo is_float(lcg_value())?'f':'n';") :to-equal "f")
  (expect (%php-run-capture "<?php $x=5; echo settype($x,'string')?'ok':'f';") :to-equal "ok")
  (expect (%php-run-capture "<?php echo is_array(sscanf('a b','%s %s'))?'arr':'x';") :to-equal "arr")
  (expect (%php-run-capture "<?php class C{} echo is_array(class_implements(new C()))?'arr':'x';") :to-equal "arr")
  (expect (%php-run-capture "<?php $loader=function($c){}; echo spl_autoload_register($loader)?'t':'f'; echo spl_autoload_unregister($loader)?'u':'n';") :to-equal "tu"))


(it-sequential "php-e2e-math-builtins-are-symbol-registered"
  (dolist (name '("sin" "cos" "tan" "log" "exp" "asin" "acos" "atan"
                  "sinh" "cosh" "tanh" "is_nan" "mt_srand" "srand"))
    (expect (cl-cc/php::%php-lookup-builtin-symbol name) :to-be-truthy))
  (expect (%php-run-capture "<?php echo round(sin('1.5707963267948966'));") :to-equal "1")
  (expect (%php-run-capture "<?php echo round(log('8', '2'));") :to-equal "3"))


(it-sequential "php-e2e-deprecated-each-is-absent"
  (expect (%php-run-capture
                   "<?php echo function_exists('each')?'present':'absent';") :to-equal "absent"))


(it-sequential "php-e2e-nl-langinfo-locale-metadata"
  (expect (%php-run-capture
                   "<?php echo function_exists('nl_langinfo')?'present':'absent';
echo ':'.nl_langinfo(CODESET);
echo ':'.nl_langinfo(ABDAY_1);
echo ':'.nl_langinfo(DAY_2);
echo ':'.nl_langinfo(ABMON_12);
echo ':'.nl_langinfo(MON_12);
echo ':'.nl_langinfo(RADIXCHAR);
echo ':'.nl_langinfo(THOUSEP);") :to-equal "present:UTF-8:Sun:Monday:Dec:December:.:,"))


(it-sequential "php-e2e-header-response-model"
  (expect (%php-run-capture
                   "<?php echo function_exists('header')?'present':'absent';
echo ':'.(function_exists('headers_list')?'present':'absent');
echo ':'.(function_exists('headers_sent')?'present':'absent');") :to-equal "present:present:present")
  (let ((cl-cc/php::*php-http-response-code* 200)
        (cl-cc/php::*php-http-headers* nil)
        (cl-cc/php::*php-output-started-p* nil))
    (expect (%php-run-capture
                     "<?php $old = http_response_code(201);
header('HTTP/1.1 404 Not Found');
echo $old.':'.http_response_code().':'.(headers_sent()?'true':'false');") :to-equal "200:404:false"))
  (let ((cl-cc/php::*php-http-response-code* 200)
        (cl-cc/php::*php-http-headers* nil)
        (cl-cc/php::*php-output-started-p* nil))
    (expect (%php-run-capture
                     "<?php header('X-Test: a');
header('X-Test: b');
header('Set-Cookie: a=1', false);
header('Set-Cookie: b=2', false);
echo json_encode(headers_list());") :to-equal "[\"X-Test: b\",\"Set-Cookie: a=1\",\"Set-Cookie: b=2\"]")))


(it-sequential "php-e2e-extract-static-array-literal"
  (expect (%php-run-capture
                   "<?php echo function_exists('extract')?'present':'absent';") :to-equal "present")
  (expect (%php-run-capture
                   "<?php extract(['a'=>1,'b'=>'two','_c'=>3,'bad-key'=>4,5=>6]); echo $a.':'.$b.':'.$_c.':'.(isset($bad)?'y':'n');") :to-equal "1:two:3:n")
  (expect (%php-run-capture
                   "<?php $a='old'; extract(['a'=>'new']); echo $a;") :to-equal "new"))


(it-sequential "php-e2e-empty-undefined-variable"
  (expect (%php-run-capture
                   "<?php echo empty($missing)?'empty':'set';") :to-equal "empty")
  (expect (%php-run-capture
                   "<?php $a=0; $b='value'; $c=null; echo (empty($a)?'empty':'set').':'.(empty($b)?'empty':'set').':'.(empty($c)?'empty':'set');") :to-equal "empty:set:empty"))


(it-sequential "php-e2e-scanf-reads-standard-input"
  (expect (%php-run-capture
                   "<?php echo function_exists('scanf')?'present':'absent';") :to-equal "present")
  (let ((*standard-input* (make-string-input-stream "12 bob 3.5")))
    (expect (%php-run-capture
                     "<?php $v=scanf('%d %s %f'); echo $v[0].':'.$v[1].':'.$v[2];") :to-equal "12:bob:3.5"))
  (let ((*standard-input* (make-string-input-stream "42-ada")))
    (expect (%php-run-capture
                     "<?php $r=scanf('%d-%s',$id,$name); echo $r.':'.$id.':'.$name;") :to-equal "2:42:ada")))


(it-sequential "php-e2e-standard-stream-constants-are-backed-by-streams"
  (expect (%php-run-capture
                   "<?php echo (STDIN===null?'bad':'in'); echo ':'; fwrite(STDOUT,'out'); echo ':'; echo (STDERR===null?'bad':'err');") :to-equal "in:out:err"))


(it-sequential "php-e2e-compact-captures-static-visible-variables"
  (expect (%php-run-capture
                   "<?php echo function_exists('compact')?'present':'absent';") :to-equal "present")
  (expect (%php-run-capture
                   "<?php $name='Ada'; $age=36; $r=compact('name','age','missing'); echo $r['name'].':'.$r['age'];") :to-equal "Ada:36")
  (expect (%php-run-capture
                   "<?php $x='yes'; $r=compact(['x']); echo $r['x'];") :to-equal "yes")
  (expect (%php-run-capture
                   "<?php $x='ok'; $y=7; $r=compact(['x',['y']]); echo $r['x'].':'.$r['y'];") :to-equal "ok:7"))


(it-sequential "php-e2e-settype-mutates-variable"
  (expect (%php-run-capture
                   "<?php $x=5; settype($x,'string'); echo gettype($x).':'.$x;") :to-equal "string:5")
  (expect (%php-run-capture
                   "<?php $x='12abc'; settype($x,'integer'); echo gettype($x).':'.$x;") :to-equal "integer:12")
  (expect (%php-run-capture
                   "<?php $x='0'; settype($x,'bool'); echo gettype($x).':'.($x?'true':'false');") :to-equal "boolean:false")
  (expect (%php-run-capture
                   "<?php $x='value'; settype($x,'null'); echo gettype($x).':'.$x;") :to-equal "NULL:")
  (expect (%php-run-capture
                   "<?php $x=7; settype($x,'array'); echo gettype($x).':'.$x[0];") :to-equal "array:7")
  (expect (%php-run-capture
                   "<?php $x=7; settype($x,'object'); echo gettype($x).':'.get_class($x).':'.$x->scalar;") :to-equal "object:stdClass:7")
  (expect (%php-run-capture
                   "<?php $x=['name'=>'ada']; settype($x,'object'); echo gettype($x).':'.get_class($x).':'.$x->name;") :to-equal "object:stdClass:ada")
  (expect (%php-run-capture
                   "<?php $x=9; echo settype($x,'bogus')?'ok':'fail'; echo ':'.$x;") :to-equal "fail:9"))


(it-sequential "php-e2e-sscanf-format-parsing"
  (expect (%php-run-capture
                   "<?php $v=sscanf('12 bob 3.5','%d %s %f'); echo $v[0].':'.$v[1].':'.$v[2];") :to-equal "12:bob:3.5")
  (expect (%php-run-capture
                   "<?php $r=sscanf('12-bob','%d-%s',$id,$name); echo $r.':'.$id.':'.$name;") :to-equal "2:12:bob")
  (expect (%php-run-capture
                   "<?php $v=sscanf('abcdef','%2c%2s'); echo $v[0].':'.$v[1];") :to-equal "ab:cd")
  (expect (%php-run-capture
                   "<?php $v=sscanf('ff 010','%x %i'); echo $v[0].':'.$v[1];") :to-equal "255:8")
  (expect (%php-run-capture
                   "<?php $r=sscanf('42 nope','%d:%s',$n,$s); echo $r.':'.$n.':'.$s;") :to-equal "1:42:"))


(it-sequential "php-e2e-intval-base"
  (expect (%php-run-capture "<?php echo intval('42');") :to-equal "42")
  (expect (%php-run-capture "<?php echo intval('1A',16);") :to-equal "26")
  (expect (%php-run-capture "<?php echo intval('0x1A',16);") :to-equal "26")
  (expect (%php-run-capture "<?php echo intval('077',8);") :to-equal "63")
  (expect (%php-run-capture "<?php echo intval('1010',2);") :to-equal "10")
  (expect (%php-run-capture "<?php echo intval('0x1A',0);") :to-equal "26")
  (expect (%php-run-capture "<?php echo intval('017',0);") :to-equal "15")
  (expect (%php-run-capture "<?php echo intval('0b101',0);") :to-equal "5")
  (expect (%php-run-capture "<?php echo intval('-FF',16);") :to-equal "-255")
  (expect (%php-run-capture "<?php echo intval('42abc');") :to-equal "42")
  (expect (%php-run-capture "<?php echo intval('42',10);") :to-equal "42"))


(it-sequential "php-e2e-gmdate-weekday"
  (expect (%php-run-capture "<?php echo gmdate('D',0);") :to-equal "Thu")
  (expect (%php-run-capture "<?php echo gmdate('l',0);") :to-equal "Thursday")
  (expect (%php-run-capture "<?php echo gmdate('N',0);") :to-equal "4")
  (expect (%php-run-capture "<?php echo gmdate('w',0);") :to-equal "4")
  (expect (%php-run-capture "<?php echo gmdate('D',86400*3);") :to-equal "Sun")
  (expect (%php-run-capture "<?php echo gmdate('w',86400*3);") :to-equal "0")
  (expect (%php-run-capture "<?php echo gmdate('N',86400*3);") :to-equal "7")
  (expect (%php-run-capture "<?php echo gmdate('g:i A',3661);") :to-equal "1:01 AM")
  (expect (%php-run-capture "<?php echo gmdate('Y-m-d H:i:s',86400);") :to-equal "1970-01-02 00:00:00")
  (expect (%php-run-capture "<?php echo gmdate('l, F j, Y',0);") :to-equal "Thursday, January 1, 1970"))


(it-sequential "php-e2e-gmdate-formats"
  (expect (%php-run-capture "<?php echo gmdate('L',0);") :to-equal "0")
  (expect (%php-run-capture "<?php echo gmdate('L',63072000);") :to-equal "1")
  (expect (%php-run-capture "<?php echo gmdate('t',0);") :to-equal "31")
  (expect (%php-run-capture "<?php echo gmdate('t',2678400);") :to-equal "28")
  (expect (%php-run-capture "<?php echo gmdate('t',65750400);") :to-equal "29")
  (expect (%php-run-capture "<?php echo gmdate('z',0);") :to-equal "0")
  (expect (%php-run-capture "<?php echo gmdate('z',86400);") :to-equal "1")
  (expect (%php-run-capture "<?php echo gmdate('jS',0);") :to-equal "1st")
  (expect (%php-run-capture "<?php echo gmdate('jS',86400);") :to-equal "2nd")
  (expect (%php-run-capture "<?php echo gmdate('jS',172800);") :to-equal "3rd")
  (expect (%php-run-capture "<?php echo gmdate('jS',864000);") :to-equal "11th")
  (expect (%php-run-capture "<?php echo gmdate('jS',1728000);") :to-equal "21st"))


(it-sequential "php-e2e-date-construct-and-udiff"
  (expect (%php-run-capture "<?php echo gmmktime(0,0,0,1,2,1970);") :to-equal "86400")
  (expect (%php-run-capture "<?php echo gmdate('Y-m-d',mktime(0,0,0,12,25,2000));") :to-equal "2000-12-25")
  (expect (%php-run-capture "<?php echo json_encode(array_values(array_udiff([1,2,3,4],[2,4],fn($a,$b)=>$a-$b)));") :to-equal "[1,3]")
  (expect (%php-run-capture "<?php echo json_encode(array_values(array_uintersect([1,2,3,4],[2,4,5],fn($a,$b)=>$a-$b)));") :to-equal "[2,4]")
  (expect (%php-run-capture "<?php echo json_encode(array_values(array_udiff(['A','b','C'],['a','B'],fn($x,$y)=>strcasecmp($x,$y))));") :to-equal "[\"C\"]"))

(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
