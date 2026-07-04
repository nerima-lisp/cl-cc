(in-package :cl-cc/test)

(in-suite cl-cc-php-e2e-suite)

(deftest php-runtime-yield-helper-preserves-value
  "The PHP yield helpers preserve yielded values for later generator lowering."
  (assert-equal '(:yield 42) (cl-cc/php:%php-yield 42))
  (assert-equal '(:yield-from (1 2)) (cl-cc/php:%php-yield-from '(1 2))))


(deftest php-runtime-exception-payload-matches-class
  "The PHP exception payload helper preserves class metadata for catch dispatch."
  (let ((payload (cl-cc/php:%php-make-exception 'ex :value)))
    (assert-true (cl-cc/php:%php-exception-object-p payload))
    (assert-eq :value (cl-cc/php:%php-exception-value payload))
    (assert-true (cl-cc/php:%php-exception-matches-p payload 'ex))
    (assert-true (cl-cc/php:%php-exception-matches-p payload '(other ex)))
    (assert-false (cl-cc/php:%php-exception-matches-p payload 'other))))


(deftest php-runtime-concat-stringifies-values
  "PHP interpolation concat stringifies null, booleans, numbers, and strings."
  (assert-string= "Hello 42" (cl-cc/php:%php-concat "Hello " 42))
  (assert-string= "x1" (cl-cc/php:%php-concat "x" t))
  (assert-string= "x" (cl-cc/php:%php-concat "x" cl-cc/php:+php-null+)))


(deftest php-e2e-var-dump-and-export
  "var_dump PRINTS a type-annotated dump (it does not return a string), and
var_export prints a re-parseable representation through symbol-registered
builtins."
  (assert-string= "int(42)"        (%php-run-capture "<?php var_dump(42);"))
  (assert-string= "string(2) \"hi\"" (%php-run-capture "<?php var_dump('hi');"))
  (assert-string= "bool(true)"     (%php-run-capture "<?php var_dump(true);"))
  (assert-string= "float(1.5)"     (%php-run-capture "<?php var_dump(1.5);"))
  (assert-string= "NULL"           (%php-run-capture "<?php var_dump(null);"))
  ;; nested array dump in PHP's multi-line format
  (assert-string= (format nil "array(2) {~%  [0]=>~%  int(1)~%  [1]=>~%  int(2)~%}")
                  (%php-run-capture "<?php var_dump([1,2]);"))
  ;; var_export re-parseable forms
  (assert-string= "42"    (%php-run-capture "<?php var_export(42);"))
  (assert-string= "'hi'"  (%php-run-capture "<?php var_export('hi');"))
  (assert-string= "true"  (%php-run-capture "<?php var_export(true);"))
  (assert-string= (format nil "array (~%  0 => 1,~%  1 => 2,~%)")
                  (%php-run-capture "<?php var_export([1,2]);"))
  ;; return-mode var_export
  (assert-string= "7"     (%php-run-capture "<?php echo var_export(7, true);")))


(deftest php-e2e-math-non-cl-named-builtins
  "Math builtins whose names are NOT CL functions (fmod, atan2, log10, log2,
hypot, deg2rad, rad2deg, base_convert).  These were registered as LAMBDAs, so
the builtin dispatch — which lowers a call to a function SYMBOL and resolves it
only when fbound — left no fbound symbol and hit 'Undefined function'.  sin/cos/
log/exp escaped the bug only by colliding with inherited CL function names.  Now
registered by named %php- symbol."
  ;; fmod: remainder, sign follows the dividend (NOT CL mod, which would give 2)
  (assert-string= "1"  (%php-run-capture "<?php echo fmod(7,3);"))
  (assert-string= "-1" (%php-run-capture "<?php echo fmod(-7,3);"))
  ;; atan2(1,1) = pi/4
  (assert-string= "0.7854" (%php-run-capture "<?php echo round(atan2(1,1),4);"))
  (assert-string= "3"  (%php-run-capture "<?php echo log10(1000);"))
  (assert-string= "3"  (%php-run-capture "<?php echo log2(8);"))
  (assert-string= "5"  (%php-run-capture "<?php echo hypot(3,4);"))
  (assert-string= "3.14159" (%php-run-capture "<?php echo round(deg2rad(180),5);"))
  (assert-string= "180" (%php-run-capture "<?php echo round(rad2deg(3.141592653589793),2);"))
  ;; base_convert: lowercase digits, both directions
  (assert-string= "11111111" (%php-run-capture "<?php echo base_convert('ff',16,2);"))
  (assert-string= "ff" (%php-run-capture "<?php echo base_convert('255',10,16);"))
  ;; is_finite / is_infinite dispatch through symbol-registered builtins.
  (assert-string= "y" (%php-run-capture "<?php echo is_finite(1.5)?'y':'n';"))
  (assert-string= "y" (%php-run-capture "<?php echo is_infinite(fdiv(1,0))?'y':'n';")))


(deftest php-e2e-serialize-unserialize
  "serialize/unserialize produce and parse PHP's native serialization format
through recursive symbol-registered helpers."
  ;; scalars
  (assert-string= "i:42;"        (%php-run-capture "<?php echo serialize(42);"))
  (assert-string= "s:5:\"hello\";" (%php-run-capture "<?php echo serialize('hello');"))
  (assert-string= "b:1;"         (%php-run-capture "<?php echo serialize(true);"))
  (assert-string= "b:0;"         (%php-run-capture "<?php echo serialize(false);"))
  (assert-string= "N;"           (%php-run-capture "<?php echo serialize(null);"))
  (assert-string= "d:3.14;"      (%php-run-capture "<?php echo serialize(3.14);"))
  ;; arrays — list and assoc, matching PHP's exact format
  (assert-string= "a:3:{i:0;i:1;i:1;i:2;i:2;i:3;}"
                  (%php-run-capture "<?php echo serialize([1,2,3]);"))
  (assert-string= "a:2:{s:1:\"a\";i:1;s:1:\"b\";i:2;}"
                  (%php-run-capture "<?php echo serialize(['a'=>1,'b'=>2]);"))
  ;; objects — public properties only, with class metadata
  (assert-string= "O:1:\"C\":1:{s:1:\"x\";i:7;}"
                  (%php-run-capture "<?php class C { public $x = 7; } echo serialize(new C());"))
  (assert-string= "O:1:\"C\":2:{s:1:\"y\";i:9;s:1:\"x\";i:7;}"
                  (%php-run-capture "<?php class C { public $x = 7; public $y = 9; function __sleep(){ return ['y','x']; } } echo serialize(new C());"))
  (assert-string= "15"
                  (%php-run-capture "<?php class C { public $x = 7; public $y = 9; function __sleep(){ return ['y']; } function __wakeup(){ $this->x = 6; } } $u = unserialize(serialize(new C())); echo $u->x + $u->y;"))
  (assert-string= "O:1:\"C\":2:{s:1:\"y\";i:9;s:1:\"x\";i:7;}"
                  (%php-run-capture "<?php class C { public $x = 7; public $y = 9; function __serialize(){ return ['y'=>$this->y, 'x'=>$this->x]; } } echo serialize(new C());"))
  (assert-string= "15"
                  (%php-run-capture "<?php class C { public $x = 7; public $y = 9; function __serialize(){ return ['y'=>$this->y, 'x'=>$this->x]; } function __unserialize($data){ $this->y = $data['y']; $this->x = $data['x'] - 1; } } $u = unserialize(serialize(new C())); echo $u->x + $u->y;"))
  ;; round-trip: scalars and nested arrays
  (assert-string= "50" (%php-run-capture "<?php echo unserialize(serialize(42))+8;"))
  (assert-string= "v"  (%php-run-capture "<?php $x=unserialize(serialize([1,2,['k'=>'v']])); echo $x[2]['k'];"))
  (assert-string= "7"  (%php-run-capture "<?php class C { public $x = 7; } $u = unserialize(serialize(new C())); echo $u->x;"))
  (assert-string= "T"  (%php-run-capture "<?php echo unserialize('b:1;')?'T':'F';"))
  ;; malformed input -> false
  (assert-string= "F"  (%php-run-capture "<?php echo unserialize('garbage')?'T':'F';")))


(deftest php-e2e-string-escape-preservation
  "PHP double-quoted strings keep the backslash for UNRECOGNIZED escapes (PHP
semantics), while still processing recognized ones.  Previously every unknown
escape dropped its backslash, so \"/\\d/\" lexed to \"/d/\" and regex character
classes (\\d \\w \\s) never matched."
  ;; unrecognized escapes keep the backslash
  (assert-string= "a\\.b"  (%php-run-capture "<?php echo \"a\\.b\";"))
  (assert-string= "\\d\\w" (%php-run-capture "<?php echo \"\\d\\w\";"))
  ;; recognized escapes still process
  (assert-string= "3" (%php-run-capture "<?php echo strlen(\"a\\nb\");"))   ; a + newline + b
  (assert-string= "A" (%php-run-capture "<?php echo \"\\x41\";"))           ; hex
  (assert-string= "H" (%php-run-capture "<?php echo \"\\u{48}\";"))         ; unicode
  (assert-string= "1" (%php-run-capture "<?php echo strlen(\"\\t\");")))    ; tab


(deftest php-e2e-md5-sha1-builtins
  "md5/sha1 return PHP-compatible digests, including raw binary output."
  (assert-string= "900150983cd24fb0d6963f7d28e17f72"
                  (%php-run-capture "<?php echo md5('abc');"))
  (assert-string= "d41d8cd98f00b204e9800998ecf8427e"
                  (%php-run-capture "<?php echo md5('');"))
  (assert-string= "900150983cd24fb0d6963f7d28e17f72"
                  (%php-run-capture "<?php echo bin2hex(md5('abc', true));"))
  (assert-string= "a9993e364706816aba3e25717850c26c9cd0d89d"
                  (%php-run-capture "<?php echo sha1('abc');"))
  (assert-string= "da39a3ee5e6b4b0d3255bfef95601890afd80709"
                  (%php-run-capture "<?php echo sha1('');"))
  (assert-string= "a9993e364706816aba3e25717850c26c9cd0d89d"
                  (%php-run-capture "<?php echo bin2hex(sha1('abc', true));")))


(deftest php-e2e-crc32-builtin
  "crc32 returns PHP-compatible IEEE CRC-32 integer values."
  (assert-string= "0" (%php-run-capture "<?php echo crc32('');"))
  (assert-string= "891568578" (%php-run-capture "<?php echo crc32('abc');"))
  (assert-string= "2356372769" (%php-run-capture "<?php echo crc32('foo');")))


(deftest php-e2e-preg-replace-callback
  "preg_replace_callback / _array.  preg_replace_callback called a non-existent
%php-regex-search and never stripped the /.../ delimiters, so every call raised
'The function %PHP-REGEX-SEARCH is undefined.'  preg_replace_callback_array had
the wrong arity (separate patterns/callbacks args instead of one map)."
  ;; the callback receives $matches[0] = full match and returns the replacement
  (assert-string= "a2b4" (%php-run-capture "<?php echo preg_replace_callback('/\\d/', fn($m)=>$m[0]*2, 'a1b2');"))
  (assert-string= "HI THERE" (%php-run-capture "<?php echo preg_replace_callback('/[a-z]+/', fn($m)=>strtoupper($m[0]), 'hi there');"))
  ;; limit caps the number of replacements
  (assert-string= "XX34" (%php-run-capture "<?php echo preg_replace_callback('/\\d/', fn($m)=>'X', '1234', 2);"))
  ;; works with the double-quoted pattern too (escape-preservation fix)
  (assert-string= "a2b4" (%php-run-capture "<?php echo preg_replace_callback(\"/\\d/\", fn($m)=>$m[0]*2, 'a1b2');"))
  ;; callback receives capture groups as $matches[1..]
  (assert-string= "x:12 y:34" (%php-run-capture "<?php echo preg_replace_callback('/(\\w+)=(\\d+)/', fn($m)=>$m[1].':'.$m[2], 'x=12 y=34');"))
  (assert-string= "4-2" (%php-run-capture "<?php echo preg_replace_callback('/((\\d)(\\d))/', fn($m)=>$m[2].'-'.$m[3], '42');"))
  ;; preg_replace_callback_array: a single [pattern => callback] map
  (assert-string= "LNLN" (%php-run-capture "<?php echo preg_replace_callback_array(['/\\d/'=>fn($m)=>'N','/[a-z]/'=>fn($m)=>'L'], 'a1b2');"))
  (assert-string= "1a 2b" (%php-run-capture "<?php echo preg_replace_callback_array(['/(\\w)(\\d)/'=>fn($m)=>$m[2].$m[1]], 'a1 b2');")))


(deftest php-e2e-preg-capture-groups
  "Capture groups in the regex engine: $1/$2/${1}/\\1/$0 backreferences in
preg_replace, and a corrected preg_match_all count.  The greedy non-backtracking
matcher now records each capturing group's span, so backreferences resolve."
  ;; $1/$2 swap and reorder
  (assert-string= "badc"  (%php-run-capture "<?php echo preg_replace('/(\\w)(\\w)/', '$2$1', 'abcd');"))
  (assert-string= "34/12" (%php-run-capture "<?php echo preg_replace('/(\\d+)-(\\d+)/', '$2/$1', '12-34');"))
  ;; $0 = whole match, ${1} brace form
  (assert-string= "a[12]b" (%php-run-capture "<?php echo preg_replace('/\\d+/', '[$0]', 'a12b');"))
  (assert-string= "hi!"    (%php-run-capture "<?php echo preg_replace('/(\\w+)/', '${1}!', 'hi');"))
  ;; nested groups number left-to-right by opening paren
  (assert-string= "4-2"    (%php-run-capture "<?php echo preg_replace('/((\\d)(\\d))/', '$2-$3', '42');"))
  ;; preg_match_all counts ALL matches (was 1 — anchored from index 0)
  (assert-string= "3" (%php-run-capture "<?php echo preg_match_all('/\\d/', '1a2b3');"))
  (assert-string= "3" (%php-run-capture "<?php echo preg_match_all('/\\w+/', 'foo bar baz');"))
  (assert-string= "0" (%php-run-capture "<?php echo preg_match_all('/\\d/', 'abc');")))


(deftest php-e2e-preg-match-out-param
  "preg_match($p,$s,$m) / preg_match_all populate the $matches out-parameter in
the caller's variable (a FRESH variable — no pre-declaration needed).  A
call-site transform assigns $m = the matches array (returned by value) and
yields the count.  Relies on ast-setq auto-declaring an unknown variable as a
global; a ref box could not be used (the VM copies host structs across the
bridge)."
  ;; capture groups land in $m[1], $m[2], full match in $m[0]
  (assert-string= "12|34" (%php-run-capture "<?php preg_match('/(\\d+)-(\\d+)/', '12-34', $m); echo $m[1].'|'.$m[2];"))
  (assert-string= "1:123:123" (%php-run-capture "<?php $r=preg_match('/(\\d+)/', 'abc123', $m); echo $r.':'.$m[0].':'.$m[1];"))
  (assert-string= "bob at host" (%php-run-capture "<?php preg_match('/(\\w+)@(\\w+)/', 'bob@host', $m); echo $m[1].' at '.$m[2];"))
  ;; the return value is still the count (0 when no match)
  (assert-string= "0" (%php-run-capture "<?php echo preg_match('/\\d/', 'abc', $m);"))
  ;; preg_match_all populates $matches in PREG_PATTERN_ORDER
  (assert-string= "1,2,3" (%php-run-capture "<?php preg_match_all('/(\\d)/', '1a2b3', $m); echo implode(',', $m[1]);"))
  (assert-string= "12,34" (%php-run-capture "<?php preg_match_all('/\\d+/', 'a12b34', $m); echo implode(',', $m[0]);"))
  ;; optional offset is preserved by the out-param lowering for both helpers
  (assert-string= "1:2" (%php-run-capture "<?php $r=preg_match('/\\d/', 'a1b2c3', $m, 0, 3); echo $r.':'.$m[0];"))
  (assert-string= "2,3" (%php-run-capture "<?php preg_match_all('/(\\d)/', 'a1b2c3', $m, 0, 3); echo implode(',', $m[1]);")))


(deftest php-e2e-ucwords-delimiters
  "ucwords uppercases the first letter of each word.  The default delimiter set
was the CL literal \" \\t\\r\\n\\f\\v\", which CL does NOT escape — it read as the
letters trnfv, so any 'r' counted as a word boundary and ucwords('world')
returned 'WorLd'.  The defaults are now built from real control characters."
  (assert-string= "Hello World"      (%php-run-capture "<?php echo ucwords('hello world');"))
  ;; words containing r/t/n/f/v must NOT get interior capitals
  (assert-string= "World Order Roar" (%php-run-capture "<?php echo ucwords('world order roar');"))
  (assert-string= "Fluffy Vivid"     (%php-run-capture "<?php echo ucwords('fluffy vivid');"))
  ;; existing capitals are preserved (ucwords only touches word-initial chars)
  (assert-string= "The QUICK Brown"  (%php-run-capture "<?php echo ucwords('the QUICK brown');"))
  ;; explicit custom delimiter still works
  (assert-string= "Hello-World"      (%php-run-capture "<?php echo ucwords('hello-world', '-');")))


(deftest php-e2e-wordwrap-cut
  "wordwrap with the cut-long-words flag force-breaks a word longer than the
width into width-sized pieces.  The flag was declared ignored, so over-long
words were never broken."
  (assert-string= "aaa|aaa" (%php-run-capture "<?php echo wordwrap('aaaaaa',3,'|',true);"))
  (assert-string= "a|very|long|word|b" (%php-run-capture "<?php echo wordwrap('a verylongword b',4,'|',true);"))
  (assert-string= "A very|long|wooooooo|ord." (%php-run-capture "<?php echo wordwrap('A very long woooooooord.',8,'|',true);"))
  ;; without cut, an over-long word overflows its own line (no leading break)
  (assert-string= "aaaaaa" (%php-run-capture "<?php echo wordwrap('aaaaaa',3,'|',false);"))
  ;; ordinary space-wrapping unaffected
  (assert-string= "aaa|bbb|ccc" (%php-run-capture "<?php echo wordwrap('aaa bbb ccc',5,'|');"))
  (assert-string= "The quick|brown fox" (%php-run-capture "<?php echo wordwrap('The quick brown fox',10,'|');")))


(deftest php-e2e-json-encode-pretty
  "json_encode honours JSON_PRETTY_PRINT (4-space indent, newlines, space after
the object colon).  The flags argument was previously declared ignored, so the
flag produced compact output."
  ;; compact output unchanged (no flag)
  (assert-string= "{\"a\":1,\"b\":[2,3]}"
                  (%php-run-capture "<?php echo json_encode(['a'=>1,'b'=>[2,3]]);"))
  ;; pretty object with a nested array
  (assert-string= (format nil "{~%    \"a\": 1,~%    \"b\": [~%        2,~%        3~%    ]~%}")
                  (%php-run-capture "<?php echo json_encode(['a'=>1,'b'=>[2,3]],JSON_PRETTY_PRINT);"))
  ;; pretty list
  (assert-string= (format nil "[~%    1,~%    2,~%    3~%]")
                  (%php-run-capture "<?php echo json_encode([1,2,3],JSON_PRETTY_PRINT);"))
  ;; empty array stays inline even in pretty mode
  (assert-string= (format nil "{~%    \"x\": [],~%    \"y\": 1~%}")
                  (%php-run-capture "<?php echo json_encode(['x'=>[],'y'=>1],JSON_PRETTY_PRINT);")))


(deftest php-e2e-json-decode
  "json_decode parses JSON objects, strings, nested structures, and scalars.
The decoder was fundamentally broken: parse-string returned an empty fresh
stream (every string -> \"\"), and the parse functions never threaded the cursor
(faked it with (+ pos 1)), so objects and any multi-char/nested value failed.
Objects decode to associative arrays."
  (assert-string= "3"       (%php-run-capture "<?php $d=json_decode('{\"a\":1,\"b\":2}',true); echo $d['a']+$d['b'];"))
  (assert-string= "Bob-30"  (%php-run-capture "<?php $d=json_decode('{\"name\":\"Bob\",\"age\":30}',true); echo $d['name'].'-'.$d['age'];"))
  (assert-string= "3"       (%php-run-capture "<?php $d=json_decode('{\"x\":{\"y\":[1,2,3]}}',true); echo $d['x']['y'][2];"))
  (assert-string= "bb3"     (%php-run-capture "<?php $d=json_decode('[\"a\",\"bb\",\"ccc\"]'); echo $d[1].strlen($d[2]);"))
  ;; scalars and malformed input
  (assert-string= "y"       (%php-run-capture "<?php echo json_decode('true')?'y':'n';"))
  (assert-string= "null"    (%php-run-capture "<?php echo json_decode('not json')===null?'null':'x';"))
  ;; round-trip through encode
  (assert-string= "alice:editor:y"
                  (%php-run-capture "<?php $o=['user'=>'alice','roles'=>['admin','editor'],'active'=>true]; $r=json_decode(json_encode($o),true); echo $r['user'].':'.$r['roles'][1].':'.($r['active']?'y':'n');")))


(deftest php-e2e-number-format-rounding
  "number_format rounds half AWAY FROM ZERO (PHP semantics), not banker's
half-to-even.  It used CL ROUND, so number_format(2.5) gave 2 and
number_format(1234.5) gave 1,234 instead of 3 and 1,235."
  (assert-string= "1,235"  (%php-run-capture "<?php echo number_format(1234.5);"))
  (assert-string= "3"      (%php-run-capture "<?php echo number_format(2.5);"))
  (assert-string= "1"      (%php-run-capture "<?php echo number_format(0.5);"))
  (assert-string= "1,234"  (%php-run-capture "<?php echo number_format(1234.4);"))
  (assert-string= "-1,235" (%php-run-capture "<?php echo number_format(-1234.5);"))
  ;; decimals and custom separators unaffected
  (assert-string= "3.14"       (%php-run-capture "<?php echo number_format(3.14159,2);"))
  (assert-string= "1,234.57"   (%php-run-capture "<?php echo number_format(1234.567,2);"))
  (assert-string= "1.234,50"   (%php-run-capture "<?php echo number_format(1234.5,2,',','.');")))


(deftest php-e2e-round-half-away
  "round() rounds half AWAY FROM ZERO (PHP), not banker's half-to-even.  It used
CL ROUND, so round(2.5) gave 2 and round(-2.5) gave -2 instead of 3 and -3."
  (assert-string= "3"    (%php-run-capture "<?php echo round(2.5);"))
  (assert-string= "4"    (%php-run-capture "<?php echo round(3.5);"))
  (assert-string= "-3"   (%php-run-capture "<?php echo round(-2.5);"))
  (assert-string= "1"    (%php-run-capture "<?php echo round(0.5);"))
  (assert-string= "2"    (%php-run-capture "<?php echo round(2.4);"))
  (assert-string= "-2"   (%php-run-capture "<?php echo round(-2.4);"))
  ;; precision (positive and negative) unaffected / correct
  (assert-string= "3.14" (%php-run-capture "<?php echo round(3.14159,2);"))
  (assert-string= "1.96" (%php-run-capture "<?php echo round(1.95583,2);"))
  (assert-string= "1242000" (%php-run-capture "<?php echo round(1241757,-3);")))


(deftest php-e2e-sprintf-flags
  "sprintf: the + flag prints a leading + on non-negative numbers; the 'X flag
sets a custom pad character; %e/%E use the e/E exponent marker (CL prints a
double-float's exponent as d/D)."
  ;; %e / %E exponent marker
  (assert-string= "1.234568e+4" (%php-run-capture "<?php echo sprintf('%e',12345.678);"))
  (assert-string= "1.200000E-4" (%php-run-capture "<?php echo sprintf('%E',0.00012);"))
  ;; + flag
  (assert-string= "+5 -5"  (%php-run-capture "<?php echo sprintf('%+d %+d',5,-5);"))
  (assert-string= "+0042"  (%php-run-capture "<?php echo sprintf('%+05d',42);"))
  (assert-string= "-0042"  (%php-run-capture "<?php echo sprintf('%05d',-42);"))
  ;; custom pad character
  (assert-string= "********42" (%php-run-capture "<?php echo sprintf(\"%'*10d\",42);"))
  (assert-string= "--------hi" (%php-run-capture "<?php echo sprintf(\"%'-10s\",'hi');"))
  ;; regressions: existing behaviour unchanged
  (assert-string= "00042"   (%php-run-capture "<?php echo sprintf('%05d',42);"))
  (assert-string= "[ab   ]"  (%php-run-capture "<?php echo sprintf('[%-5s]','ab');"))
  (assert-string= "3.14"     (%php-run-capture "<?php echo sprintf('%.2f',3.14159);"))
  (assert-string= "ff FF 10" (%php-run-capture "<?php echo sprintf('%x %X %o',255,255,8);")))


(deftest php-e2e-base-conversions
  "dechex/hexdec/decbin/bindec/decoct/octdec.  They were registered as LAMBDAs
(non-CL-named), which the symbol-based builtin dispatch could not resolve
('Undefined function: DECHEX'); now named %php-* helpers registered by symbol."
  (assert-string= "ff"   (%php-run-capture "<?php echo dechex(255);"))
  (assert-string= "255"  (%php-run-capture "<?php echo hexdec('ff');"))
  (assert-string= "1010" (%php-run-capture "<?php echo decbin(10);"))
  (assert-string= "10"   (%php-run-capture "<?php echo bindec('1010');"))
  (assert-string= "100"  (%php-run-capture "<?php echo decoct(64);"))
  (assert-string= "64"   (%php-run-capture "<?php echo octdec('100');"))
  ;; negatives use 64-bit two's complement, like PHP
  (assert-string= "ffffffffffffffff" (%php-run-capture "<?php echo dechex(-1);"))
  (assert-string= "48879" (%php-run-capture "<?php echo hexdec(dechex(48879));")))


(deftest php-e2e-max-min
  "max/min accept a single array argument or multiple arguments and compare with
PHP's <=> (numeric strings numerically).  They previously applied CL MAX, which
errored on an array, a string, or mixed types."
  (assert-string= "3"  (%php-run-capture "<?php echo max([3,1,2]);"))
  (assert-string= "1"  (%php-run-capture "<?php echo min([3,1,2]);"))
  (assert-string= "5"  (%php-run-capture "<?php echo max(1,5,3);"))
  (assert-string= "10" (%php-run-capture "<?php echo max(1,'10',5);"))
  (assert-string= "2"  (%php-run-capture "<?php echo min(2.5,2,3);"))
  (assert-string= "42" (%php-run-capture "<?php echo max(42);")))


(deftest php-e2e-symbol-registered-builtins
  "Builtins that were registered as LAMBDAs (non-CL-named) and so hit 'Undefined
function' when called — iterator_to_array/iterator_count, lcg_value,
settype, sscanf, spl_autoload_register/unregister/functions,
class_implements/parents/uses, echo/print — are now named %php-* helpers registered by
symbol and are callable."
  (cl-cc/php::%php-register-all-builtins)
  (assert-eq 'cl-cc/php::%php-echo (cl-cc/php::%php-lookup-builtin-symbol "echo"))
  (assert-eq 'cl-cc/php::%php-print (cl-cc/php::%php-lookup-builtin-symbol "print"))
  (assert-string= "ab"
                  (with-output-to-string (out)
                    (let ((*standard-output* out))
                      (funcall (cl-cc/php::%php-lookup-builtin "echo") "a" "b"))))
  (assert-string= "c1"
                  (with-output-to-string (out)
                    (let ((*standard-output* out))
                      (princ (funcall (cl-cc/php::%php-lookup-builtin "print") "c")))))
  (assert-string= "2" (%php-run-capture "<?php function g(){yield 1;yield 2;} echo count(iterator_to_array(g()));"))
  (assert-string= "3" (%php-run-capture "<?php function g(){yield 1;yield 2;yield 3;} echo iterator_count(g());"))
  (assert-string= "f" (%php-run-capture "<?php echo is_float(lcg_value())?'f':'n';"))
  (assert-string= "ok" (%php-run-capture "<?php $x=5; echo settype($x,'string')?'ok':'f';"))
  (assert-string= "arr" (%php-run-capture "<?php echo is_array(sscanf('a b','%s %s'))?'arr':'x';"))
  (assert-string= "arr" (%php-run-capture "<?php class C{} echo is_array(class_implements(new C()))?'arr':'x';"))
  (assert-string= "tu" (%php-run-capture "<?php $loader=function($c){}; echo spl_autoload_register($loader)?'t':'f'; echo spl_autoload_unregister($loader)?'u':'n';")))


(deftest php-e2e-math-builtins-are-symbol-registered
  "Math builtins resolve through named helper symbols."
  (dolist (name '("sin" "cos" "tan" "log" "exp" "asin" "acos" "atan"
                  "sinh" "cosh" "tanh" "is_nan" "mt_srand" "srand"))
    (assert-true (cl-cc/php::%php-lookup-builtin-symbol name)))
  (assert-string= "1" (%php-run-capture "<?php echo round(sin('1.5707963267948966'));"))
  (assert-string= "3" (%php-run-capture "<?php echo round(log('8', '2'));")))


(deftest php-e2e-deprecated-each-is-absent
  "PHP each() is intentionally absent from the builtin registry."
  (assert-string= "absent"
                  (%php-run-capture
                   "<?php echo function_exists('each')?'present':'absent';")))


(deftest php-e2e-nl-langinfo-locale-metadata
  "nl_langinfo() exposes deterministic locale metadata and predefined item constants."
  (assert-string= "present:UTF-8:Sun:Monday:Dec:December:.:,"
                  (%php-run-capture
                   "<?php echo function_exists('nl_langinfo')?'present':'absent';
echo ':'.nl_langinfo(CODESET);
echo ':'.nl_langinfo(ABDAY_1);
echo ':'.nl_langinfo(DAY_2);
echo ':'.nl_langinfo(ABMON_12);
echo ':'.nl_langinfo(MON_12);
echo ':'.nl_langinfo(RADIXCHAR);
echo ':'.nl_langinfo(THOUSEP);")))


(deftest php-e2e-header-response-model
  "header()/headers_list()/headers_sent() maintain a deterministic CLI response model."
  (assert-string= "present:present:present"
                  (%php-run-capture
                   "<?php echo function_exists('header')?'present':'absent';
echo ':'.(function_exists('headers_list')?'present':'absent');
echo ':'.(function_exists('headers_sent')?'present':'absent');"))
  (let ((cl-cc/php::*php-http-response-code* 200)
        (cl-cc/php::*php-http-headers* nil)
        (cl-cc/php::*php-output-started-p* nil))
    (assert-string= "200:404:false"
                    (%php-run-capture
                     "<?php $old = http_response_code(201);
header('HTTP/1.1 404 Not Found');
echo $old.':'.http_response_code().':'.(headers_sent()?'true':'false');")))
  (let ((cl-cc/php::*php-http-response-code* 200)
        (cl-cc/php::*php-http-headers* nil)
        (cl-cc/php::*php-output-started-p* nil))
    (assert-string= "[\"X-Test: b\",\"Set-Cookie: a=1\",\"Set-Cookie: b=2\"]"
                    (%php-run-capture
                     "<?php header('X-Test: a');
header('X-Test: b');
header('Set-Cookie: a=1', false);
header('Set-Cookie: b=2', false);
echo json_encode(headers_list());"))))


(deftest php-e2e-extract-static-array-literal
  "extract() is exposed and static array literals bind caller variables."
  (assert-string= "present"
                  (%php-run-capture
                   "<?php echo function_exists('extract')?'present':'absent';"))
  (assert-string= "1:two:3:n"
                  (%php-run-capture
                   "<?php extract(['a'=>1,'b'=>'two','_c'=>3,'bad-key'=>4,5=>6]); echo $a.':'.$b.':'.$_c.':'.(isset($bad)?'y':'n');"))
  (assert-string= "new"
                  (%php-run-capture
                   "<?php $a='old'; extract(['a'=>'new']); echo $a;")))


(deftest php-e2e-empty-undefined-variable
  "empty($x) treats an undefined variable as empty without evaluating it first."
  (assert-string= "empty"
                  (%php-run-capture
                   "<?php echo empty($missing)?'empty':'set';"))
  (assert-string= "empty:set:empty"
                  (%php-run-capture
                   "<?php $a=0; $b='value'; $c=null; echo (empty($a)?'empty':'set').':'.(empty($b)?'empty':'set').':'.(empty($c)?'empty':'set');")))


(deftest php-e2e-scanf-reads-standard-input
  "scanf reads standard input with the sscanf parser and supports out-params."
  (assert-string= "present"
                  (%php-run-capture
                   "<?php echo function_exists('scanf')?'present':'absent';"))
  (let ((*standard-input* (make-string-input-stream "12 bob 3.5")))
    (assert-string= "12:bob:3.5"
                    (%php-run-capture
                     "<?php $v=scanf('%d %s %f'); echo $v[0].':'.$v[1].':'.$v[2];")))
  (let ((*standard-input* (make-string-input-stream "42-ada")))
    (assert-string= "2:42:ada"
                    (%php-run-capture
                     "<?php $r=scanf('%d-%s',$id,$name); echo $r.':'.$id.':'.$name;"))))


(deftest php-e2e-standard-stream-constants-are-backed-by-streams
  "STDIN/STDOUT/STDERR are real stream handles."
  (assert-string= "in:out:err"
                  (%php-run-capture
                   "<?php echo (STDIN===null?'bad':'in'); echo ':'; fwrite(STDOUT,'out'); echo ':'; echo (STDERR===null?'bad':'err');")))


(deftest php-e2e-compact-captures-static-visible-variables
  "compact() captures visible variables for static string/name-list arguments."
  (assert-string= "present"
                  (%php-run-capture
                   "<?php echo function_exists('compact')?'present':'absent';"))
  (assert-string= "Ada:36"
                  (%php-run-capture
                   "<?php $name='Ada'; $age=36; $r=compact('name','age','missing'); echo $r['name'].':'.$r['age'];"))
  (assert-string= "yes"
                  (%php-run-capture
                   "<?php $x='yes'; $r=compact(['x']); echo $r['x'];"))
  (assert-string= "ok:7"
                  (%php-run-capture
                   "<?php $x='ok'; $y=7; $r=compact(['x',['y']]); echo $r['x'].':'.$r['y'];")))


(deftest php-e2e-settype-mutates-variable
  "settype mutates its first argument by reference."
  (assert-string= "string:5"
                  (%php-run-capture
                   "<?php $x=5; settype($x,'string'); echo gettype($x).':'.$x;"))
  (assert-string= "integer:12"
                  (%php-run-capture
                   "<?php $x='12abc'; settype($x,'integer'); echo gettype($x).':'.$x;"))
  (assert-string= "boolean:false"
                  (%php-run-capture
                   "<?php $x='0'; settype($x,'bool'); echo gettype($x).':'.($x?'true':'false');"))
  (assert-string= "NULL:"
                  (%php-run-capture
                   "<?php $x='value'; settype($x,'null'); echo gettype($x).':'.$x;"))
  (assert-string= "array:7"
                  (%php-run-capture
                   "<?php $x=7; settype($x,'array'); echo gettype($x).':'.$x[0];"))
  (assert-string= "object:stdClass:7"
                  (%php-run-capture
                   "<?php $x=7; settype($x,'object'); echo gettype($x).':'.get_class($x).':'.$x->scalar;"))
  (assert-string= "object:stdClass:ada"
                  (%php-run-capture
                   "<?php $x=['name'=>'ada']; settype($x,'object'); echo gettype($x).':'.get_class($x).':'.$x->name;"))
  (assert-string= "fail:9"
                  (%php-run-capture
                   "<?php $x=9; echo settype($x,'bogus')?'ok':'fail'; echo ':'.$x;")))


(deftest php-e2e-sscanf-format-parsing
  "sscanf parses common PHP scanf directives and lowers out-params to caller
assignments."
  (assert-string= "12:bob:3.5"
                  (%php-run-capture
                   "<?php $v=sscanf('12 bob 3.5','%d %s %f'); echo $v[0].':'.$v[1].':'.$v[2];"))
  (assert-string= "2:12:bob"
                  (%php-run-capture
                   "<?php $r=sscanf('12-bob','%d-%s',$id,$name); echo $r.':'.$id.':'.$name;"))
  (assert-string= "ab:cd"
                  (%php-run-capture
                   "<?php $v=sscanf('abcdef','%2c%2s'); echo $v[0].':'.$v[1];"))
  (assert-string= "255:8"
                  (%php-run-capture
                   "<?php $v=sscanf('ff 010','%x %i'); echo $v[0].':'.$v[1];"))
  (assert-string= "1:42:"
                  (%php-run-capture
                   "<?php $r=sscanf('42 nope','%d:%s',$n,$s); echo $r.':'.$n.':'.$s;")))


(deftest php-e2e-intval-base
  "intval(string, base) parses in the given base; the base argument was ignored,
so intval('1A',16) gave 1 and intval('077',8) gave 77.  Base 0 autodetects from
a 0x/0b/0o/leading-0 prefix."
  (assert-string= "42"  (%php-run-capture "<?php echo intval('42');"))
  (assert-string= "26"  (%php-run-capture "<?php echo intval('1A',16);"))
  (assert-string= "26"  (%php-run-capture "<?php echo intval('0x1A',16);"))
  (assert-string= "63"  (%php-run-capture "<?php echo intval('077',8);"))
  (assert-string= "10"  (%php-run-capture "<?php echo intval('1010',2);"))
  ;; base 0 autodetect
  (assert-string= "26"  (%php-run-capture "<?php echo intval('0x1A',0);"))
  (assert-string= "15"  (%php-run-capture "<?php echo intval('017',0);"))
  (assert-string= "5"   (%php-run-capture "<?php echo intval('0b101',0);"))
  ;; negative, trailing junk, explicit base 10
  (assert-string= "-255" (%php-run-capture "<?php echo intval('-FF',16);"))
  (assert-string= "42"   (%php-run-capture "<?php echo intval('42abc');"))
  (assert-string= "42"   (%php-run-capture "<?php echo intval('42',10);")))


(deftest php-e2e-gmdate-weekday
  "gmdate day-of-week was off by one (decode-universal-time uses 0=Monday, but
PHP w is 0=Sunday and N is 1=Monday..7=Sunday), so gmdate('D',0) gave Wed for
1970-01-01 (a Thursday).  Also adds the g (12-hour, no leading zero) format."
  (assert-string= "Thu"      (%php-run-capture "<?php echo gmdate('D',0);"))
  (assert-string= "Thursday" (%php-run-capture "<?php echo gmdate('l',0);"))
  (assert-string= "4"        (%php-run-capture "<?php echo gmdate('N',0);"))
  (assert-string= "4"        (%php-run-capture "<?php echo gmdate('w',0);"))
  ;; Sunday (1970-01-04): w=0, N=7
  (assert-string= "Sun"      (%php-run-capture "<?php echo gmdate('D',86400*3);"))
  (assert-string= "0"        (%php-run-capture "<?php echo gmdate('w',86400*3);"))
  (assert-string= "7"        (%php-run-capture "<?php echo gmdate('N',86400*3);"))
  ;; g = 12-hour without leading zero
  (assert-string= "1:01 AM"  (%php-run-capture "<?php echo gmdate('g:i A',3661);"))
  ;; existing date parts unaffected
  (assert-string= "1970-01-02 00:00:00" (%php-run-capture "<?php echo gmdate('Y-m-d H:i:s',86400);"))
  (assert-string= "Thursday, January 1, 1970" (%php-run-capture "<?php echo gmdate('l, F j, Y',0);")))


(deftest php-e2e-gmdate-formats
  "gmdate L (leap year), t (days in month), z (day of year, 0-based), and S
(ordinal suffix) format characters, which were previously emitted literally."
  (assert-string= "0"  (%php-run-capture "<?php echo gmdate('L',0);"))        ; 1970 not leap
  (assert-string= "1"  (%php-run-capture "<?php echo gmdate('L',63072000);")) ; 1972 leap
  (assert-string= "31" (%php-run-capture "<?php echo gmdate('t',0);"))        ; January
  (assert-string= "28" (%php-run-capture "<?php echo gmdate('t',2678400);"))  ; Feb 1970
  (assert-string= "29" (%php-run-capture "<?php echo gmdate('t',65750400);")) ; Feb 1972 (leap)
  (assert-string= "0"  (%php-run-capture "<?php echo gmdate('z',0);"))        ; Jan 1
  (assert-string= "1"  (%php-run-capture "<?php echo gmdate('z',86400);"))    ; Jan 2
  ;; ordinal suffixes
  (assert-string= "1st"  (%php-run-capture "<?php echo gmdate('jS',0);"))
  (assert-string= "2nd"  (%php-run-capture "<?php echo gmdate('jS',86400);"))
  (assert-string= "3rd"  (%php-run-capture "<?php echo gmdate('jS',172800);"))
  (assert-string= "11th" (%php-run-capture "<?php echo gmdate('jS',864000);"))
  (assert-string= "21st" (%php-run-capture "<?php echo gmdate('jS',1728000);")))


(deftest php-e2e-date-construct-and-udiff
  "gmmktime (GMT alias of mktime) plus array_udiff/array_uintersect (diff and
intersect with a user comparison callback) — all were missing."
  (assert-string= "86400"      (%php-run-capture "<?php echo gmmktime(0,0,0,1,2,1970);"))
  (assert-string= "2000-12-25" (%php-run-capture "<?php echo gmdate('Y-m-d',mktime(0,0,0,12,25,2000));"))
  (assert-string= "[1,3]"      (%php-run-capture "<?php echo json_encode(array_values(array_udiff([1,2,3,4],[2,4],fn($a,$b)=>$a-$b)));"))
  (assert-string= "[2,4]"      (%php-run-capture "<?php echo json_encode(array_values(array_uintersect([1,2,3,4],[2,4,5],fn($a,$b)=>$a-$b)));"))
  ;; callback-driven (case-insensitive) comparison
  (assert-string= "[\"C\"]"    (%php-run-capture "<?php echo json_encode(array_values(array_udiff(['A','b','C'],['a','B'],fn($x,$y)=>strcasecmp($x,$y))));")))

(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
