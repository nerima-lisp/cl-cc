;;;; helpers-e2e.lisp — Former fixture helpers for PHP compiler integration tests.
;;;;
;;;; The PHP test system no longer loads this file.

(in-package :cl-cc/test)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %php-run-capture (source)
    "Compile PHP SOURCE to VM and run it, returning everything it echoed as a
string. compile-string with :language :php registers the PHP host bridges, so a
fresh VM state runs the program end-to-end."
    (let* ((result  (cl-cc/compile:compile-string source :target :vm :language :php))
           (program (cl-cc/compile:compilation-result-program result))
           (out     (make-string-output-stream)))
      (cl-cc/vm:run-compiled program :output-stream out)
      ;; Trim a trailing newline the VM appends when flushing program output.
      (string-right-trim '(#\Newline) (get-output-stream-string out)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %php-run-capture-io (source &key ini-settings)
    "Compile PHP SOURCE and capture both stdout and error output."
    (let ((cl-cc/php::*php-ini-settings* ini-settings)
          (cl-cc/php::*php-error-reporting-level* 32767))
      (let* ((result  (cl-cc/compile:compile-string source :target :vm :language :php))
             (program (cl-cc/compile:compilation-result-program result))
             (out     (make-string-output-stream))
             (err     (make-string-output-stream)))
        (handler-case
            (let ((*error-output* err))
              (cl-cc/vm:run-compiled program :output-stream out))
          (error (c)
            (declare (ignore c))))
        (values (string-right-trim '(#\Newline)
                                   (get-output-stream-string out))
                (string-right-trim '(#\Newline)
                                   (get-output-stream-string err)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %php-make-ini-settings (&rest pairs)
    "Create a fresh INI settings table for tests."
    (let ((table (make-hash-table :test 'equal)))
      (dolist (entry cl-cc/php::*php-ini-defaults* table)
        (setf (gethash (car entry) table) (cdr entry)))
      (loop for (key value) on pairs by #'cddr
            do (setf (gethash key table) value))
      table)))

;; t/parser-class-e2e-test.lisp:9 "php-e2e-static-members: ~S"
(it-sequential-each
      (("<?php class M{ static function sq($x){ return $x*$x; } } echo M::sq(5);"
        "25")
       ("<?php class C{ static $n=5; } echo C::$n;"
        "5")
       ("<?php class C{ static $n=1; } C::$n=7; echo C::$n;"
        "7")
       ("<?php class C{ static $n=2; } C::$n += 5; echo C::$n;"
        "7")
       ("<?php class C{ static $n=1; } echo C::$n++; echo ':'.C::$n;"
        "1:2")
       ("<?php class C{ static $n=1; } echo ++C::$n; echo ':'.C::$n;"
        "2:2")
       ("<?php class C{ static $n=1; static function inc(){ self::$n += 2; return self::$n; } } echo C::inc().':'.C::$n;"
        "3:3")
       ("<?php class C{ const PI=3; static $n=5; } echo C::PI.'|'.C::$n;"
        "3|5")
       ("<?php class U{ static function inc($x){ return $x+1; } static function dbl($x){ return $x*2; } } echo U::inc(U::dbl(10));"
        "21"))
      "php-e2e-static-members: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:33 "php-e2e-self-static-parent: ~S"
(it-sequential-each
      (("<?php class C{ static function f(){ return self::g(); } static function g(){ return 9; } } echo C::f();"
        "9")
       ("<?php class C{ static function f(){ return static::g(); } static function g(){ return 7; } } echo C::f();"
        "7")
       ("<?php class C{ const X=42; static function get(){ return self::X; } } echo C::get();"
        "42")
       ("<?php class F{ static function fac($n){ return $n<=1?1:$n*F::fac($n-1); } } echo F::fac(5);"
        "120")
       ("<?php class F{ static function fac($n){ return $n<=1?1:$n*self::fac($n-1); } } echo F::fac(6);"
        "720")
       ("<?php class A{ static function who(){ return 1; } } class B extends A{ static function who2(){ return parent::who()+10; } } echo B::who2();"
        "11")
       ("<?php class K{ public $base=100; static function tag(){ return 9; } function show(){ return $this->base+K::tag(); } } $o=new K(); echo $o->show();"
        "109"))
      "php-e2e-self-static-parent: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:53 "php-e2e-constructor: ~S"
(it-sequential-each
      (("<?php class C{ function __construct(){ echo 'made'; } } $o=new C();"
        "made")
       ("<?php class C{ public $x; function __construct($v){ $this->x=$v; } } $o=new C(9); echo $o->x;"
        "9")
       ("<?php class P{ public $n; public $a; function __construct($n,$a){ $this->n=$n; $this->a=$a; } } $p=new P('Bob',30); echo $p->n.':'.$p->a;"
        "Bob:30")
       ("<?php class C{ public $x; function __construct($v){ $this->x=$v; } function dbl(){ return $this->x*2; } } $o=new C(5); echo $o->dbl();"
        "10")
       ("<?php class C{ public $x=5; } $o=new C(); echo $o->x;"
        "5"))
      "php-e2e-constructor: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:69 "php-e2e-clone-object: ~S"
(it-sequential-each
      (("<?php class C{ public $x=1; } $a=new C(); $b=clone $a; $b->x=9; echo $a->x.':'.$b->x;"
        "1:9")
       ("<?php class C{ public $x; function __construct($x){ $this->x=$x; } function __clone(){ $this->x=$this->x+10; } } $a=new C(4); $b=clone $a; echo $a->x.':'.$b->x;"
        "4:14"))
      "php-e2e-clone-object: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:79 "php-e2e-constructor-promotion: ~S"
(it-sequential-each
      (("<?php class C{ function __construct(public $x){} } $o=new C(42); echo $o->x;"
        "42")
       ("<?php class P{ function __construct(public $n, public $v){} } $p=new P('hello',99); echo $p->n.':'.$p->v;"
        "hello:99")
       ("<?php class M{ function __construct(public $x, $label){ echo $this->x.':'.$label; } } $o=new M(5,'extra');"
        "5:extra")
       ("<?php class C{ function __construct(public $v){ $this->v=$this->v*2; } } $o=new C(5); echo $o->v;"
        "10"))
      "php-e2e-constructor-promotion: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:93 "php-e2e-instance-method-this: ~S"
(it-sequential-each
      (("<?php class C{ public $x=3; function g(){ return $this->x; } } $o=new C(); echo $o->g();"
        "3")
       ("<?php class C{ public $b=10; function add($a){ return $this->b+$a; } } $o=new C(); echo $o->add(5);"
        "15")
       ("<?php class C{ public $n=0; function setN($v){ $this->n=$v; } } $o=new C(); $o->setN(7); echo $o->n;"
        "7")
       ("<?php class C{ public $c=0; function inc(){ $this->c=$this->c+1; return $this->c; } } $o=new C(); $o->inc(); echo $o->inc();"
        "2")
       ("<?php class C{ public $v=1; function get(){ return $this->v; } function me(){ return $this; } } $o=new C(); echo $o->me()->get();"
        "1")
       ("<?php class C{ function g(){ return 42; } } $o=new C(); echo $o->g();"
        "42"))
      "php-e2e-instance-method-this: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:111 "php-e2e-class-property-access: ~S"
(it-sequential-each
      (("<?php class C{ public $x=7; } $o=new C(); echo $o->x;"
        "7")
       ("<?php class C{ public $x; } $o=new C(); $o->x=5; echo $o->x;"
        "5")
       ("<?php class C{ public $x=7; } $o=new C(); echo \"x={$o->x}\";"
        "x=7")
       ("<?php class C{ public $s='hi'; } $o=new C(); echo $o->s;"
        "hi"))
      "php-e2e-class-property-access: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:150 "php-e2e-uninitialized-property-null: ~S"
(it-sequential-each
      (("<?php class C{public $x;} $o=new C(); echo is_null($o->x)?'yes':'no';"
        "yes")
       ("<?php class C{public $x;} $o=new C(); echo $o->x ?? 'def';"
        "def")
       ("<?php class C{public $x=7;} $o=new C(); echo $o->x ?? 'def';"
        "7")
       ("<?php class C{public $s='hi';} $o=new C(); echo $o->s;"
        "hi")
       ("<?php class C{public $x;} $o=new C(); $o->x=9; echo $o->x;"
        "9"))
      "php-e2e-uninitialized-property-null: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:166 "php-e2e-unset-object-property: ~S"
(it-sequential-each
      (("<?php class C{public $x=7;} $o=new C(); unset($o->x); echo is_null($o->x)?'null':'set';"
        "null")
       ("<?php class C{public $x='x';} $o=new C(); unset($o->x); echo $o->x ?? 'D';"
        "D")
       ("<?php class C{public $x=1; public $y=2;} $o=new C(); $a=[9]; unset($a[0], $o->y); echo $o->x.':'.(is_null($o->y)?'null':'set').':'.count($a);"
        "1:null:0"))
      "php-e2e-unset-object-property: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:178 "php-e2e-enum-name-value-cases: ~S"
(it-sequential-each
      (("<?php enum S{case A;} echo S::A->name;"
        "A")
       ("<?php enum S:string{case A='a';case B='b';} echo S::A->value;"
        "a")
       ("<?php enum S:int{case A=10;} echo S::A->value;"
        "10")
       ("<?php enum S{case A;case B;} echo count(S::cases());"
        "2")
       ("<?php enum S{case A;case B;} $n=''; foreach(S::cases() as $c){$n.=$c->name;} echo $n;"
        "AB")
       ("<?php enum S:int{case A=1;case B=2;} echo S::from(2)->name;"
        "B")
       ("<?php enum S:int{case A=1;} echo S::tryFrom(9)===null?'null':'f';"
        "null")
       ("<?php enum S{case A;} echo S::A===S::A?'y':'n';"
        "y")
       ("<?php enum Suit:string{case H='h';case S='s';} $x=Suit::H; echo match($x){Suit::H=>'hearts',Suit::S=>'spades'};"
        "hearts"))
      "php-e2e-enum-name-value-cases: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:202 "php-e2e-enum-methods: ~S"
(it-sequential-each
      (("<?php enum S{case A; public function label(){return 'L';}} echo S::A->label();"
        "L")
       ("<?php enum S:string{case A='a'; public function up(){return strtoupper($this->value);}} echo S::A->up();"
        "A")
       ("<?php enum Suit{case H;case S; public function color(){return match($this){Suit::H=>'red',Suit::S=>'black'};}} echo Suit::H->color();"
        "red")
       ("<?php enum S{case A; public function add($n){return $n+1;}} echo S::A->add(5);"
        "6")
       ("<?php enum S{case A; public function x(){return 1;}} echo S::A->name;"
        "A")
       ("<?php enum S{case A; const X=5; public function y(){return 2;}} echo S::X;"
        "5"))
      "php-e2e-enum-methods: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:220 "php-e2e-nullsafe-operator: ~S"
(it-sequential-each
      (("<?php class C{public $x=5;} $o=new C(); echo $o?->x;"
        "5")
       ("<?php class C{public $x=5;} $o=null; echo $o?->x ?? 'def';"
        "def")
       ("<?php class C{function m(){return 7;}} $o=new C(); echo $o?->m();"
        "7")
       ("<?php class C{function m(){return 7;}} $o=null; echo $o?->m() ?? 'n';"
        "n")
       ("<?php class C{public $v=3; function get(){return $this->v;}} $o=new C(); echo $o?->get();"
        "3")
       ("<?php class C{public $n=null;} $o=new C(); echo $o?->n?->x ?? 'none';"
        "none")
       ("<?php class C{public $x;} $o=new C(); echo $o?->x ?? 'wasnull';"
        "wasnull"))
      "php-e2e-nullsafe-operator: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:240 "php-e2e-predefined-constants: ~S"
(it-sequential-each
      (("<?php echo PHP_INT_MAX;"
        "9223372036854775807")
       ("<?php echo PHP_INT_SIZE;"
        "8")
       ("<?php echo round(M_PI,5);"
        "3.14159")
       ("<?php echo PHP_VERSION;"
        "8.5.0")
       ("<?php echo phpversion();"
        "8.5.0")
       ("<?php echo is_nan(NAN) ? '1' : '0';"
        "1")
       ("<?php echo SORT_STRING;"
        "2")
       ("<?php echo PHP_EOL===\"\\n\"?'y':'n';"
        "y")
       ("<?php echo str_pad('5',3,'0',STR_PAD_LEFT);"
        "005")
       ("<?php echo str_pad('5',3,'0',STR_PAD_RIGHT);"
        "500")
       ("<?php echo \\PHP_MAJOR_VERSION;"
        "8")
       ("<?php echo NOT_A_REAL_CONSTANT_XYZ;"
        ""))
      "php-e2e-predefined-constants: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:275 "php-e2e-call-user-func: ~S"
(it-sequential-each
      (("<?php function sq($x){return $x*$x;} echo call_user_func('sq',5);"
        "25")
       ("<?php function add($a,$b){return $a+$b;} echo call_user_func('add',3,4);"
        "7")
       ("<?php echo call_user_func('strtoupper','hi');"
        "HI")
       ("<?php $f=function($n){return $n+100;}; echo call_user_func($f,1);"
        "101")
       ("<?php $g=fn($x)=>$x*3; echo call_user_func($g,7);"
        "21")
       ("<?php function add($a,$b){return $a+$b;} echo call_user_func_array('add',[10,20]);"
        "30")
       ("<?php function sq($x){return $x*$x;} echo call_user_func_array('sq',[6]);"
        "36"))
      "php-e2e-call-user-func: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:305 "php-e2e-class-reflection-builtins: ~S"
(it-sequential-each
      (("<?php interface ReflIfaceI{} interface ReflIfaceB extends ReflIfaceI{} class ReflImpl implements ReflIfaceB{} $s=''; foreach(class_implements(new ReflImpl()) as $v){$s.=$v;} echo $s;"
        "REFLIFACEBREFLIFACEI")
       ("<?php class ReflBase{} class ReflMid extends ReflBase{} class ReflLeaf extends ReflMid{} $s=''; foreach(class_parents(new ReflLeaf()) as $v){$s.=$v;} echo $s;"
        "REFLMIDREFLBASE")
       ("<?php class ReflStringBase{} class ReflStringChild extends ReflStringBase{} $s=''; foreach(class_parents('ReflStringChild') as $v){$s.=$v;} echo $s;"
        "REFLSTRINGBASE")
       ("<?php trait ReflTraitT{} trait ReflTraitU{} class ReflTraitUser{use ReflTraitT,ReflTraitU;} $s=''; foreach(class_uses(new ReflTraitUser()) as $v){$s.=$v;} echo $s;"
        "REFLTRAITTREFLTRAITU"))
      "php-e2e-class-reflection-builtins: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:319 "php-e2e-spl-data-structures: ~S"
(it-sequential-each
      (("<?php $s=new SplStack(); $s->push('a'); $s->push('b'); echo $s->pop().$s->top().$s->count().get_class($s); echo method_exists($s,'push')?'Y':'N';"
        "ba1SplStackY")
       ("<?php $q=new SplQueue(); $q->enqueue('a'); $q->enqueue('b'); echo $q->dequeue().$q->bottom().$q->count();"
        "ab1")
       ("<?php $f=new SplFixedArray(2); $f->offsetSet(0,'x'); $f->offsetSet(1,'y'); $f->setSize(3); $f->offsetSet(2,'z'); echo $f->offsetGet(0).$f->offsetGet(1).$f->getSize().$f->offsetGet(2);"
        "xy3z")
       ("<?php $min=new SplMinHeap(); $max=new SplMaxHeap(); foreach([3,1,2] as $v){$min->insert($v);$max->insert($v);} echo $min->extract().$max->extract();"
        "13"))
      "php-e2e-spl-data-structures: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-class-e2e-test.lisp:142 "php-compile-enum-static-builtins"
(it-sequential "php-compile-enum-static-builtins"
  (let ((forms (cl-cc/php:parse-php-source
                "<?php enum Status: int { case Draft = 0; case Published = 1; } $a = Status::Published; $b = Status::from(1); $c = Status::tryFrom(99); $d = Status::cases();")))
    (expect (= 2 (length forms)) :to-be-truthy)
    (expect (cl-cc/ast:ast-let-p (second forms)) :to-be-truthy)))

;; t/parser-class-e2e-test.lisp:270 "php-e2e-predefined-constants-are-not-functions"
(it-sequential "php-e2e-predefined-constants-are-not-functions"
  (expect (%php-run-capture
                   "<?php echo function_exists('PHP_EOL')?'present':'absent'; echo ':'; echo function_exists('SORT_STRING')?'present':'absent'; echo ':'; echo function_exists('null')?'present':'absent';") :to-equal "absent:absent:absent"))

;; t/parser-class-e2e-test.lisp:295 "php-e2e-spl-autoload-default-loader-is-absent"
(it-sequential "php-e2e-spl-autoload-default-loader-is-absent"
  (expect (%php-run-capture
                   "<?php echo function_exists('spl_autoload')?'present':'absent';") :to-equal "absent"))

;; t/parser-class-e2e-test.lisp:300 "php-e2e-spl-autoload-registry"
(it-sequential "php-e2e-spl-autoload-registry"
  (expect (%php-run-capture
                   "<?php function clcc_loader_a($c){} function clcc_loader_b($c){} spl_autoload_unregister('clcc_loader_a'); spl_autoload_unregister('clcc_loader_b'); echo spl_autoload_register('clcc_loader_a')?'R':'F'; echo spl_autoload_register('clcc_loader_b', true, true)?'P':'F'; $list=spl_autoload_functions(); echo ':'.count($list).':'.$list[0].':'.$list[1]; echo ':'.(spl_autoload_unregister('clcc_loader_a')?'U':'N'); $list=spl_autoload_functions(); $found=''; foreach($list as $fn){$found.=$fn.',';} echo ':'.$found; echo ':'.(spl_autoload_unregister('clcc_loader_a')?'again':'missing'); spl_autoload_unregister('clcc_loader_b');") :to-equal "RP:2:clcc_loader_b:clcc_loader_a:U:clcc_loader_b,:missing"))

;; t/parser-class-e2e-test.lisp:332 "php-e2e-trait-method-is-actually-callable"
(it-sequential
    "php-e2e-trait-method-is-actually-callable"
    ;; Every existing trait e2e test before this used EMPTY traits and only checked
    ;; class_uses() reflection (which does work — trait NAMES are tracked). None ever
    ;; called an actual trait method, which is the entire point of a trait — and the most
    ;; basic possible case (one trait, one method, no conflicts) previously failed outright
    ;; with a CLOS "slot is missing" error: `use Greetable;` produced only an internal
    ;; :PHP-TRAIT-USE marker slot-def that class lowering never acted on. Fixed by
    ;; %PHP-MERGE-TRAIT-MEMBERS (src/parser-trait.lisp), which replaces that marker with
    ;; the used traits' actual member slot-defs before the class's AST-DEFCLASS is built.
    (expect (%php-run-capture
             "<?php trait Greetable { function greet() { return 'hi'; } } class C { use Greetable; } $o = new C(); echo $o->greet();")
            :to-equal "hi"))

;; t/parser-class-e2e-test.lisp:346 "php-e2e-trait-method-instead-of-property-still-works-when-mixed"
(it-sequential
    "php-e2e-trait-method-instead-of-property-still-works-when-mixed"
    ;; A trait can also declare properties, not just methods — %PHP-MERGE-TRAIT-MEMBERS
    ;; copies every member uniformly, not just method-shaped slot-defs.
    (expect (%php-run-capture
             "<?php trait HasName { public $name = 'anon'; function label() { return 'name:'.$this->name; } }
class P { use HasName; }
$p = new P(); echo $p->label();")
            :to-equal "name:anon"))

;; t/parser-class-e2e-test.lisp:356 "php-e2e-two-non-conflicting-traits-both-merge-in"
(it-sequential
    "php-e2e-two-non-conflicting-traits-both-merge-in"
    (expect (%php-run-capture
             "<?php trait A { function a() { return 'a'; } } trait B { function b() { return 'b'; } }
class C { use A, B; }
$c = new C(); echo $c->a().$c->b();")
            :to-equal "ab"))

;; t/parser-class-e2e-test.lisp:364 "php-e2e-conflicting-trait-methods-resolved-by-insteadof"
(it-sequential
    "php-e2e-conflicting-trait-methods-resolved-by-insteadof"
    (expect (%php-run-capture
             "<?php trait A { function hello() { return 'A'; } } trait B { function hello() { return 'B'; } }
class C { use A, B { A::hello insteadof B; } }
$c = new C(); echo $c->hello();")
            :to-equal "A"))

;; t/parser-class-e2e-test.lisp:372 "conflicting trait methods with no insteadof clause report a clear error
instead of silently picking one trait's version"
(it-sequential
    "conflicting trait methods with no insteadof clause report a clear error
instead of silently picking one trait's version"
    ;; Real PHP treats this as a fatal "has not been applied, because there are
    ;; collisions" compile error — %PHP-MERGE-TRAIT-MEMBERS matches that rather than
    ;; letting whichever trait happens to be listed first silently win.
    (let ((condition
            (handler-case
                (progn (%php-run-capture
                        "<?php trait A { function hello() { return 'A'; } } trait B { function hello() { return 'B'; } }
class C { use A, B; }
$c = new C(); echo $c->hello();")
                       nil)
              (error (e) e))))
      (expect condition :to-be-truthy)
      (expect (search "more than one" (princ-to-string condition)) :to-be-truthy)))

;; t/parser-class-e2e-test.lisp:389 "php-e2e-trait-as-alias-adds-a-new-name-without-removing-the-original"
(it-sequential
    "php-e2e-trait-as-alias-adds-a-new-name-without-removing-the-original"
    ;; `TraitA::method as alias;' is purely additive in real PHP: the alias becomes an
    ;; extra name for the same method body, and the original name stays callable too.
    (expect (%php-run-capture
             "<?php trait A { function hello() { return 'A'; } } class C { use A { A::hello as hi; } }
$c = new C(); echo $c->hello().'-'.$c->hi();")
            :to-equal "A-A"))

;; t/parser-class-e2e-test.lisp:398 "php-e2e-trait-as-visibility-only-change-does-not-break-the-method"
(it-sequential
    "php-e2e-trait-as-visibility-only-change-does-not-break-the-method"
    ;; `method as protected;' (no rename) changes only the merged member's visibility —
    ;; not tested here is whether that visibility is actually enforced against external
    ;; callers, since this runtime's PHP visibility enforcement is a separate mechanism
    ;; this investigation did not verify; what this confirms is that the alias
    ;; visibility-only path does not corrupt or remove the member itself.
    (expect (%php-run-capture
             "<?php trait A { function hello() { return 'A'; } } class C { use A { hello as protected; } function call() { return $this->hello(); } }
$c = new C(); echo $c->call();")
            :to-equal "A"))

;; t/parser-class-e2e-test.lisp:410 "php-e2e-interface-constant-is-accessible-through-an-implementing-class"
(it-sequential
    "php-e2e-interface-constant-is-accessible-through-an-implementing-class"
    ;; t/parser-interface-test.lisp's interface-constant tests only check the parser's raw
    ;; AST shape (zero %php-run-capture calls in that whole file) — this is the first check
    ;; that a class implementing an interface can actually read a constant it declares.
    (expect (%php-run-capture
             "<?php interface HasVersion { const VERSION = '1.0'; } class Impl implements HasVersion {} echo Impl::VERSION;")
            :to-equal "1.0"))

;; t/parser-class-e2e-test.lisp:419 "php-e2e-abstract-class-basics"
(it-sequential
    "php-e2e-abstract-class-basics"
    ;; No test anywhere in this suite exercised the class-level `abstract' modifier before
    ;; this (only abstract METHODS inside interfaces/traits). Confirms the modifier at
    ;; least parses and a concrete subclass still works; does NOT assert that directly
    ;; instantiating the abstract class itself is rejected — see the next test for that,
    ;; kept separate since it may reveal an actual gap rather than just missing coverage.
    (expect (%php-run-capture
             "<?php abstract class Shape { abstract function area(): float; function describe(): string { return 'area='.$this->area(); } }
class Square extends Shape { function __construct(private float $side) {} function area(): float { return $this->side * $this->side; } }
$s = new Square(4); echo $s->describe();")
            :to-equal "area=16"))

;; t/parser-class-e2e-test.lisp:432 "instantiating an abstract class directly is currently NOT rejected —
unlike real PHP's fatal \"Cannot instantiate abstract class\" error"
(it-sequential
    "instantiating an abstract class directly is currently NOT rejected —
unlike real PHP's fatal \"Cannot instantiate abstract class\" error"
    ;; The `abstract class Foo {...}' PARSING gap fixed just above this test made this
    ;; visible: an earlier version of this same test passed for the wrong reason — the
    ;; parse error the missing :ABSTRACT statement parser caused masked the fact that,
    ;; once parsing succeeds, `new Shape()' on an abstract class is not actually
    ;; rejected at all. Nothing currently tracks "is this class abstract" as class
    ;; metadata past parsing, and no instantiation-time check exists to consult it —
    ;; a real, separately-scoped gap from the parsing fix, left undone here.
    (expect (%php-run-capture
             "<?php abstract class Shape { abstract function area(): float; } new Shape(); echo 'no error';")
            :to-equal "no error"))

;; t/parser-class-e2e-test.lisp:446 "does an enum using a trait actually get the trait's methods?"
(it-sequential
    "does an enum using a trait actually get the trait's methods?"
    ;; t/parser-class-test.lisp's "php-parser-enum-implements-methods-traits-and-constants"
    ;; combines `enum ... { use HasLabels; ... }' but only checks the raw parsed AST shape via
    ;; %PHP-FIRST, never compiles or runs anything — the exact same "looks tested, never
    ;; exercised end-to-end" pattern that hid the class-trait bug fixed earlier in this file.
    ;; %PHP-MERGE-ALL-TRAIT-MEMBERS only merges into :PHP-KIND :CLASS AST-DEFCLASS nodes (see
    ;; its docstring), not :ENUM ones — this checks whether that gap is real.
    (expect (%php-run-capture
             "<?php trait Greetable { function greet() { return 'hi'; } } enum S { use Greetable; case A; } echo S::A->greet();")
            :to-equal "hi"))

;; t/parser-class-e2e-test.lisp:458 "an abstract class using a trait, and a concrete subclass of it, both get
the trait's methods"
(it-sequential
    "an abstract class using a trait, and a concrete subclass of it, both get
the trait's methods"
    ;; Checking a plausible combination of two things fixed independently this session
    ;; (abstract-class parsing, trait-method merging) for the same kind of interaction bug
    ;; the enum+trait case had.
    (expect (%php-run-capture
             "<?php trait Greetable { function greet() { return 'hi'; } }
abstract class Base { use Greetable; abstract function extra(): string; }
class Impl extends Base { function extra(): string { return 'x'; } }
$o = new Impl(); echo $o->greet().$o->extra();")
            :to-equal "hix"))

;; t/parser-class-e2e-test.lisp:471 "php-e2e-readonly-property-can-be-set-once-during-construction"
(it-sequential
    "php-e2e-readonly-property-can-be-set-once-during-construction"
    ;; No e2e test anywhere in this suite exercised `readonly' at all before this —
    ;; only parser-level AST-shape tests confirming the modifier is captured.
    (expect (%php-run-capture
             "<?php class Point { public function __construct(public readonly int $x) {} }
$p = new Point(5); echo $p->x;")
            :to-equal "5"))

;; t/parser-class-e2e-test.lisp:480 "reassigning a readonly property after construction is currently NOT
rejected — unlike real PHP's fatal \"Cannot modify readonly property\" error"
(it-sequential
    "reassigning a readonly property after construction is currently NOT
rejected — unlike real PHP's fatal \"Cannot modify readonly property\" error"
    ;; Same class of gap as abstract-class instantiation above: the modifier parses and is
    ;; captured as AST metadata (see t/parser-class-test.lisp), but nothing enforces it at
    ;; property-write time. Documented via a test asserting the current, unenforced
    ;; behavior rather than implemented — enforcing this needs the property-write lowering
    ;; to check readonly-ness and the object's own already-initialized-or-not state, which
    ;; this investigation did not trace.
    (expect (%php-run-capture
             "<?php class Point { public function __construct(public readonly int $x) {} }
$p = new Point(5); $p->x = 99; echo $p->x;")
            :to-equal "99"))

;; t/parser-class-e2e-test.lisp:494 "php-e2e-final-class-basics"
(it-sequential
    "php-e2e-final-class-basics"
    ;; `final class Foo {...}' had no registered top-level statement parser at all before
    ;; this — the same gap `abstract class` had, fixed the same way. No test anywhere
    ;; exercised the class-level `final' modifier before this.
    (expect (%php-run-capture
             "<?php final class Sealed { function greet() { return 'hi'; } } $o = new Sealed(); echo $o->greet();")
            :to-equal "hi"))

;; t/parser-class-e2e-test.lisp:503 "extending a final class is currently NOT rejected — unlike real PHP's
fatal \"Cannot extend final class\" error"
(it-sequential
    "extending a final class is currently NOT rejected — unlike real PHP's
fatal \"Cannot extend final class\" error"
    ;; Same class of gap as abstract-class instantiation and readonly reassignment
    ;; elsewhere in this file: the modifier now parses, but nothing tracks "is this class
    ;; final" past parsing to reject an extends of it.
    (expect (%php-run-capture
             "<?php final class Sealed { function greet() { return 'hi'; } } class Broken extends Sealed {} $o = new Broken(); echo $o->greet();")
            :to-equal "hi"))

;; t/parser-class-e2e-test.lisp:513 "final method: parses and runs like an ordinary method"
(it-sequential "final method: parses and runs like an ordinary method"
    ;; :final is already in %php-parse-visibility-modifiers's accepted modifier list
    ;; alongside :abstract/:readonly, so a member-level `final function` is consumed as
    ;; an ordinary modifier before the :function dispatch -- no separate grammar entry
    ;; needed (unlike `final class`, which required its own statement-level parser).
    (expect (%php-run-capture
             "<?php class Base { final function seal() { return 'sealed'; } } $o = new Base(); echo $o->seal();")
            :to-equal "sealed"))

;; t/parser-class-e2e-test.lisp:522 "final method: overriding in a subclass is not rejected"
(it-sequential "final method: overriding in a subclass is not rejected"
    ;; Same class of gap as final-class extension and abstract-instantiation elsewhere
    ;; in this file: the modifier parses and is discarded, but nothing tracks "is this
    ;; method final" past parsing to reject an override in a subclass.
    (expect (%php-run-capture
             "<?php class Base { final function seal() { return 'A'; } } class Sub extends Base { function seal() { return 'B'; } } $o = new Sub(); echo $o->seal();")
            :to-equal "B"))

;; t/parser-e2e-test.lisp:9 "php-e2e-postfix-incdec-on-places: ~S"
(it-sequential-each
      (("<?php class C{ public $n=0; } $o=new C(); $o->n++; $o->n++; echo $o->n;"
        "2")
       ("<?php class C{ public $n=10; } $o=new C(); echo $o->n++;" "10")
       ("<?php class C{ public $n=5; } $o=new C(); $o->n--; echo $o->n;" "4")
       ("<?php class C{ public $n=0; function inc(){ $this->n++; } } $o=new C(); $o->inc(); $o->inc(); $o->inc(); echo $o->n;"
        "3")
       ("<?php $a=[0]; $a[0]++; $a[0]++; echo $a[0];" "2")
       ("<?php $a=[5]; $old=$a[0]++; echo $old.'-'.$a[0];" "5-6")
       ("<?php $x='7'; $old=$x++; echo $old.'-'.$x;" "7-8")
       ("<?php $x=null; ++$x; echo $x;" "1")
       ("<?php $x=true; --$x; echo $x;" "0"))
      "php-e2e-postfix-incdec-on-places: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:24 "php-e2e-incdec-undefined-variable: ~S"
(it-sequential-each
      (("<?php $x++; echo $x;" "1")
       ("<?php echo (++$x).':'.$x;" "1:1")
       ("<?php $x--; echo $x;" "-1")
       ("<?php echo (--$x).':'.$x;" "-1:-1"))
      "php-e2e-incdec-undefined-variable: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:53 "php-e2e-declare-directives: ~S"
(it-sequential-each
      (("<?php declare(strict_types=1); echo 7;" "7")
       ("<?php declare(ticks=1) { echo 'A'; echo 'B'; } echo 'C';" "ABC")
       ("<?php declare(ticks=1) echo 'X'; echo 'Y';" "XY")
       ("<?php declare(ticks=1): echo 'A'; echo 'B'; enddeclare; echo 'C';" "ABC"))
      "php-e2e-declare-directives: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:61 "php-e2e-output-buffering-builtins: ~S"
(it-sequential-each
      (("<?php ob_start(); echo 'a'; $s=ob_get_clean(); echo $s; echo 'b';" "ab")
       ("<?php ob_start(); echo 'x'; $s=ob_get_contents(); ob_end_clean(); echo $s.'y';"
        "xy")
       ("<?php ob_start(); echo 'a'; ob_start(); echo 'x'; ob_end_clean(); echo 'b'; echo ob_get_clean();"
        "ab")
       ("<?php ob_start(); ?>html<?php $s=ob_get_clean(); echo $s.'!';" "html!"))
      "php-e2e-output-buffering-builtins: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:71 "php-e2e-ini-get-set-and-error-reporting: ~S"
(it-sequential-each
      (("<?php echo ini_get('default_charset');" "UTF-8")
       ("<?php $old=ini_set('default_charset','Shift_JIS'); echo $old.':'.ini_get('default_charset'); ini_set('default_charset',$old);"
        "UTF-8:Shift_JIS")
       ("<?php error_reporting(32767); $old=error_reporting(0); echo $old.':'.error_reporting().':'.ini_get('error_reporting'); error_reporting($old);"
        "32767:0:0")
       ("<?php error_reporting(32767); $old=ini_set('error_reporting','5'); echo $old.':'.error_reporting().':'.ini_get('error_reporting'); error_reporting($old);"
        "32767:5:5")
       ("<?php echo ini_get('definitely_missing')===false?'false':'value';"
        "false"))
      "php-e2e-ini-get-set-and-error-reporting: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:84 "php-e2e-error-and-exception-handler-registration: ~S"
(it-sequential-each
      (("<?php function h($errno,$errstr,$file,$line){ echo 'H:'.$errno.':'.$errstr; return true; } set_error_handler('h', E_USER_WARNING); trigger_error('boom', E_USER_WARNING); restore_error_handler();"
        "H:512:boom")
       ("<?php function only_notice($errno,$errstr){ echo 'handled'; return true; } set_error_handler('only_notice', E_USER_NOTICE); trigger_error('masked', E_USER_WARNING); restore_error_handler();"
        "masked")
       ("<?php function h1($errno,$errstr){ echo 'one'; return true; } function h2($errno,$errstr){ echo 'two'; return true; } set_error_handler('h1'); $old=set_error_handler('h2'); echo $old.':'; trigger_error('x', E_USER_NOTICE); restore_error_handler(); restore_error_handler();"
        "h1:two")
       ("<?php function eh($e){ echo 'e'; } set_exception_handler('eh'); $old=set_exception_handler(function($e){ echo 'x'; }); echo $old.':ok'; restore_exception_handler(); restore_exception_handler();"
        "eh:ok"))
      "php-e2e-error-and-exception-handler-registration: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:96 "php-e2e-error-suppression-operator: ~S"
(it-sequential-each
      (("<?php @trigger_error('hidden', E_USER_NOTICE); echo 'ok';" "ok")
       ("<?php function h($errno,$errstr){ echo 'handler'; return true; } set_error_handler('h'); @trigger_error('hidden', E_USER_NOTICE); restore_error_handler(); echo 'ok';"
        "ok")
       ("<?php error_reporting(E_ALL); $v=@trigger_error('hidden', E_USER_NOTICE); echo $v.':'.error_reporting();"
        "1:32767"))
      "php-e2e-error-suppression-operator: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:105 "php-e2e-date-default-timezone-and-ini: ~S"
(it-sequential-each
      (("<?php $old=date_default_timezone_get(); date_default_timezone_set('UTC'); $prev=date_default_timezone_get(); date_default_timezone_set('Asia/Tokyo'); echo $prev.':'.date_default_timezone_get().':'.ini_get('date.timezone').':'.date('H:i',0).':'.gmdate('H:i',0).':'.date('U',0); date_default_timezone_set($old);"
        "UTC:Asia/Tokyo:Asia/Tokyo:09:00:00:00:0")
       ("<?php $old=date_default_timezone_get(); date_default_timezone_set('Asia/Tokyo'); $prev=ini_set('date.timezone','UTC'); echo $prev.':'.date_default_timezone_get().':'.ini_get('date.timezone'); date_default_timezone_set($old);"
        "Asia/Tokyo:UTC:UTC")
       ("<?php $old=date_default_timezone_get(); date_default_timezone_set('UTC'); echo (date_default_timezone_set('No/Such_Zone')?'true':'false').':'.date_default_timezone_get(); date_default_timezone_set($old);"
        "false:UTC")
       ("<?php $old=date_default_timezone_get(); date_default_timezone_set('Asia/Tokyo'); echo strtotime('1970-01-01 09:00:00').':'.mktime(18,0,0,1,1,1970).':'.date('Y-m-d H:i',0); date_default_timezone_set($old);"
        "0:32400:1970-01-01 09:00"))
      "php-e2e-date-default-timezone-and-ini: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:146 "php-e2e-list-destructuring: ~S"
(it-sequential-each
      (("<?php list($a,$b)=[1,2]; echo $a+$b;" "3")
       ("<?php list($a,$b,$c)=[10,20,30]; echo $a+$b+$c;" "60")
       ("<?php function pair(){return [4,9];} list($x,$y)=pair(); echo $x.'-'.$y;"
        "4-9")
       ("<?php list($n,$a)=['Bob',30]; echo $n.':'.$a;" "Bob:30")
       ("<?php [$a,$b]=[1,2]; echo $a.','.$b;" "1,2")
       ("<?php $a=1;$b=2; [$a,$b]=[$b,$a]; echo $a.','.$b;" "2,1"))
      "php-e2e-list-destructuring: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:157 "php-e2e-list-destructuring-nested-keyed: ~S"
(it-sequential-each
      (("<?php [[$a,$b],$c]=[[1,2],3]; echo $a+$b+$c;" "6")
       ("<?php list(list($a,$b),$c)=[[1,2],3]; echo $a+$b+$c;" "6")
       ("<?php ['x'=>$a,'y'=>$b]=['x'=>1,'y'=>2]; echo $a+$b;" "3")
       ("<?php ['y'=>$a,'x'=>$b]=['x'=>1,'y'=>2]; echo $a-$b;" "1")
       ("<?php [['k'=>$a],$b]=[['k'=>9],3]; echo $a.','.$b;" "9,3"))
      "php-e2e-list-destructuring-nested-keyed: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:166 "php-e2e-complex-string-interpolation: ~S"
(it-sequential-each
      (("<?php $a=['k'=>5]; echo \"v={$a['k']}\";" "v=5")
       ("<?php $a=['x'=>['y'=>9]]; echo \"n={$a['x']['y']}\";" "n=9")
       ("<?php class C{ function g(){ return 42; } } $o=new C(); echo \"m={$o->g()}\";"
        "m=42")
       ("<?php $n='Bob'; $a=['age'=>30]; echo \"Hi $n, age {$a['age']}!\";"
        "Hi Bob, age 30!")
       ("<?php $n='Bob'; echo \"Hi $n!\";" "Hi Bob!")
       ("<?php $n='Bob'; echo \"Hi {$n}!\";" "Hi Bob!"))
      "php-e2e-complex-string-interpolation: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:178 "php-e2e-equality-operators: ~S"
(it-sequential-each
      (("<?php function f($x){ return $x == 4; } echo f(4);" "1")
       ("<?php function f($x){ return $x == 4; } echo f(3);" "")
       ("<?php function f($x){ return $x != 4; } echo f(3);" "1")
       ("<?php function f($x){ return $x % 2 == 0; } echo f(4);" "1"))
      "php-e2e-equality-operators: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:186 "php-e2e-equality-type-juggling: ~S"
(it-sequential-each
      (("<?php $a='5'; if ($a == 5) { echo 'loose'; }" "loose")
       ("<?php $a='5'; if ($a === 5) { echo 'x'; } else { echo 'strictfail'; }"
        "strictfail"))
      "php-e2e-equality-type-juggling: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:193 "php-e2e-echo-boolean-php-semantics: ~S"
(it-sequential-each
      (("<?php echo true;" "1")
       ("<?php echo false;" "")
       ("<?php echo true . 'X';" "1X")
       ("<?php echo false . 'X';" "X")
       ("<?php echo (5 == 5);" "1")
       ("<?php echo 1,2,3;" "123"))
      "php-e2e-echo-boolean-php-semantics: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:203 "php-e2e-logical-operators: ~S"
(it-sequential-each
      (("<?php echo (true && true) ? 'T':'F';" "T")
       ("<?php echo (true && false) ? 'T':'F';" "F")
       ("<?php echo (false || true) ? 'T':'F';" "T")
       ("<?php echo (false || false) ? 'T':'F';" "F")
       ("<?php echo !false ? 'T':'F';" "T")
       ("<?php echo !5 ? 'T':'F';" "F")
       ("<?php echo true && true;" "1"))
      "php-e2e-logical-operators: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:214 "php-e2e-logical-in-conditions: ~S"
(it-sequential-each
      (("<?php $a=1; $b=2; if ($a && $b) { echo 'both'; }" "both")
       ("<?php $x=5; if ($x > 0 && $x < 10) { echo 'mid'; }" "mid")
       ("<?php $a=true; $b=false; if (!$b && $a) { echo 'ok'; }" "ok"))
      "php-e2e-logical-in-conditions: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:228 "php-e2e-default-arguments: ~S"
(it-sequential-each
      (("<?php function g($x=7){ return $x; } echo g();" "7")
       ("<?php function g($x=7){ return $x; } echo g(3);" "3")
       ("<?php function g($s='hi'){ return $s; } echo g();" "hi")
       ("<?php function f($a,$b=10){ return $a+$b; } echo f(5);" "15")
       ("<?php function f($a,$b=10){ return $a+$b; } echo f(5,2);" "7")
       ("<?php function f($a=1,$b=2,$c=3){ return $a+$b+$c; } echo f();" "6")
       ("<?php function f($a=1,$b=2,$c=3){ return $a+$b+$c; } echo f(10);" "15"))
      "php-e2e-default-arguments: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:239 "php-e2e-named-arguments: ~S"
(it-sequential-each
      (("<?php function join3($a,$b,$c){ return $a.$b.$c; } echo join3(c:'C', a:'A', b:'B');"
        "ABC")
       ("<?php function f($a='A',$b='B',$c='C'){ return $a.$b.$c; } echo f(c:'C', b:2);"
        "A2C")
       ("<?php function f($a='A',$b='B',$c='C'){ return $a.$b.$c; } echo f('X', c:'C');"
        "XBC")
       ("<?php function join3($a,$b,$c){ return $a.$b.$c; } echo join3(...['A'], b:'B', c:'C');"
        "ABC")
       ("<?php function join3($a,$b,$c){ return $a.$b.$c; } echo join3(...['A','B'], c:'C');"
        "ABC")
       ("<?php function f($a='A',$b='B',$c='C'){ return $a.$b.$c; } echo f(...['X'], c:'C');"
        "XBC")
       ("<?php class C{ function join3($a,$b,$c){ return $a.$b.$c; } } $c=new C(); echo $c->join3(c:'C', a:'A', b:'B');"
        "ABC")
       ("<?php class C{ function join3($a,$b,$c){ return $a.$b.$c; } } $c=new C(); echo $c?->join3(c:'C', a:'A', b:'B');"
        "ABC")
       ("<?php class C{ static function join3($a,$b,$c){ return $a.$b.$c; } } echo C::join3(c:'C', a:'A', b:'B');"
        "ABC")
       ("<?php class C{ function join3($a,$b,$c){ return $a.$b.$c; } } $c=new C(); echo $c->join3(...['A'], b:'B', c:'C');"
        "ABC"))
      "php-e2e-named-arguments: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:263 "php-e2e-default-arguments-all-callable-forms: ~S"
(it-sequential-each
      (("<?php $f=function($x=9){ return $x; }; echo $f();" "9")
       ("<?php $f=function($x=9){ return $x; }; echo $f(4);" "4")
       ("<?php $f=fn($x=8)=>$x*2; echo $f();" "16")
       ("<?php class C{ function m($x=5){ return $x; } } $c=new C(); echo $c->m();"
        "5")
       ("<?php class C{ function m($x=5){ return $x; } } $c=new C(); echo $c->m(20);"
        "20"))
      "php-e2e-default-arguments-all-callable-forms: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:274 "php-e2e-variadic-parameters: ~S"
(it-sequential-each
      (("<?php function s(...$n){ return array_sum($n); } echo s(1,2,3,4);" "10")
       ("<?php function s(...$n){ return count($n); } echo s(1,2,3,4);" "4")
       ("<?php function s(...$n){ return count($n); } echo s();" "0")
       ("<?php function f($a,...$rest){ return $a.':'.count($rest); } echo f('x',1,2,3);"
        "x:3")
       ("<?php function j(...$xs){ $r=''; foreach($xs as $x){ $r.=$x; } return $r; } echo j('a','b','c');"
        "abc"))
      "php-e2e-variadic-parameters: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:285 "php-e2e-variadic-all-callable-forms: ~S"
(it-sequential-each
      (("<?php $s=function(...$n){ return array_sum($n); }; echo $s(1,2,3);" "6")
       ("<?php $s=fn(...$n)=>array_sum($n); echo $s(5,5,5);" "15"))
      "php-e2e-variadic-all-callable-forms: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:291 "php-e2e-spread-call: ~S"
(it-sequential-each
      (("<?php function add($a,$b){ return $a+$b; } $args=[2,3]; echo add(...$args);"
        "5")
       ("<?php function add3($a,$b,$c){ return $a+$b+$c; } $x=[1,2,3]; echo add3(...$x);"
        "6")
       ("<?php function f($a,$b,$c){ return $a.$b.$c; } $r=['y','z']; echo f('x',...$r);"
        "xyz")
       ("<?php $a=[3,1,2]; echo max(...$a);" "3")
       ("<?php $a=[3,1,2]; echo min(...$a);" "1"))
      "php-e2e-spread-call: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:303 "php-e2e-named-args-after-dynamic-spread: ~S"
(it-sequential-each
      (("<?php function j($a,$b,$c){ return $a.$b.$c; } $args=['a','b']; echo j(...$args, c:'c');"
        "abc")
       ("<?php function j($a,$b='x',$c='z'){ return $a.$b.$c; } $args=['a']; echo j(...$args, c:'c');"
        "axc"))
      "php-e2e-named-args-after-dynamic-spread: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:311 "php-e2e-spread-composes-with-variadic-and-closures: ~S"
(it-sequential-each
      (("<?php function s(...$n){ return array_sum($n); } $a=[10,20,30]; echo s(...$a);"
        "60")
       ("<?php $f=function($a,$b){ return $a*$b; }; $args=[4,5]; echo $f(...$args);"
        "20"))
      "php-e2e-spread-composes-with-variadic-and-closures: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:319 "php-e2e-for-loop-continue: ~S"
(it-sequential-each
      (("<?php $s=0; for($i=0;$i<6;$i++){ if($i%2)continue; $s+=$i; } echo $s;"
        "6")
       ("<?php $s=0; for($i=0;$i<10;$i++){ if($i==5)break; if($i%2)continue; $s+=$i; } echo $s;"
        "6")
       ("<?php $s=0; for($i=0;$i<3;$i++){ for($j=0;$j<3;$j++){ if($j==1)continue; $s++; } } echo $s;"
        "6"))
      "php-e2e-for-loop-continue: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:329 "php-e2e-for-loop-basics: ~S"
(it-sequential-each
      (("<?php $s=0; for($i=1;$i<=4;$i++){ $s+=$i; } echo $s;" "10")
       ("<?php $s=0; for($i=0;$i<10;$i++){ if($i==3)break; $s+=$i; } echo $s;"
        "3")
       ("<?php $s=''; for($i=3;$i>0;$i--){ $s.=$i; } echo $s;" "321"))
      "php-e2e-for-loop-basics: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:369 "php-e2e-match-and-relational-booleans: ~S"
(it-sequential-each
      (("<?php $x=2; echo match($x){1=>'a',2=>'b'};" "b")
       ("<?php $x=2; echo match($x){1,2=>'low',3=>'hi'};" "low")
       ("<?php $x=3; echo match($x){1,2=>'low',3=>'hi'};" "hi")
       ("<?php $x=5; echo match(true){$x>3=>'big',default=>'small'};" "big")
       ("<?php $x=1; echo match(true){$x>3=>'big',default=>'small'};" "small")
       ("<?php echo gettype(5>3);" "boolean")
       ("<?php echo gettype(2<1);" "boolean")
       ("<?php echo (5>3)===true ? 'T':'n';" "T")
       ("<?php echo gettype(true);" "boolean")
       ("<?php echo 'apple'<'banana' ? 'y':'n';" "y")
       ("<?php $s=0; for($i=0;$i<4;$i++){$s+=$i;} echo $s;" "6")
       ("<?php echo 1<=>2;" "-1"))
      "php-e2e-match-and-relational-booleans: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:385 "php-e2e-nullish-coalescing-assignment: ~S"
(it-sequential-each
      (("<?php $a=null; $a ??= 'x'; echo $a;" "x")
       ("<?php $a='keep'; $a ??= 'x'; echo $a;" "keep")
       ("<?php $a=false; $a ??= 'x'; echo $a===false?'F':'o';" "F")
       ("<?php $a=0; $a ??= 'x'; echo $a;" "0")
       ("<?php class C{public $x;} $o=new C(); $o->x ??= 'y'; echo $o->x;" "y")
       ("<?php class C{public $x=5;} $o=new C(); $o->x ??= 'y'; echo $o->x;" "5")
       ("<?php $a=[]; $a['k'] ??= 'v'; echo $a['k'];" "v")
       ("<?php $a=['k'=>3]; $a['k'] ??= 'v'; echo $a['k'];" "3"))
      "php-e2e-nullish-coalescing-assignment: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:397 "php-e2e-compound-assign-undefined-var: ~S"
(it-sequential-each
      (("<?php $a += 3; echo $a;" "3")
       ("<?php $a -= 3; echo $a;" "-3")
       ("<?php $a *= 5; echo $a;" "0")
       ("<?php $s .= 'hi'; echo $s;" "hi")
       ("<?php $a ??= 'y'; echo $a;" "y")
       ("<?php $a=5; $a += 3; echo $a;" "8")
       ("<?php $a='7'; $a += '3'; echo $a;" "10")
       ("<?php $a='10'; $a -= true; $a -= '1'; echo $a;" "8")
       ("<?php $a='3'; $a *= '2'; echo $a;" "6")
       ("<?php $s='x'; $s .= 'y'; echo $s;" "xy"))
      "php-e2e-compound-assign-undefined-var: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:411 "php-e2e-arithmetic-operand-coercion: ~S"
(it-sequential-each
      (("<?php echo null + 3;" "3")
       ("<?php echo '5' + 3;" "8")
       ("<?php echo '4' * '2';" "8")
       ("<?php echo '1.5' + 1;" "2.5")
       ("<?php echo true + 1;" "2")
       ("<?php echo 2 - -3;" "5")
       ("<?php echo +'7';" "7")
       ("<?php echo -'7';" "-7")
       ("<?php echo -null;" "0")
       ("<?php echo -true;" "-1")
       ("<?php echo 2 + 3;" "5")
       ("<?php echo 2 + 3 * 4;" "14")
       ("<?php $a=10; $a -= 3; echo $a;" "7")
       ("<?php $s=0; for($i=1;$i<=4;$i++){$s+=$i;} echo $s;" "10"))
      "php-e2e-arithmetic-operand-coercion: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:429 "php-e2e-float-stringification: ~S"
(it-sequential-each
      (("<?php echo 1.5 * 2;" "3")
       ("<?php echo 1.5 + 0.25;" "1.75")
       ("<?php echo 3.14;" "3.14")
       ("<?php echo 6.0;" "6")
       ("<?php echo 1000000.0;" "1000000")
       ("<?php echo 10 / 4;" "2.5")
       ("<?php echo 10 / 2;" "5")
       ("<?php echo 'x=' . 1.5;" "x=1.5")
       ("<?php $f=2.5; echo \"v=$f\";" "v=2.5")
       ("<?php echo sqrt(2);" "1.4142135623731")
       ("<?php echo sqrt(4);" "2"))
      "php-e2e-float-stringification: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:444 "php-e2e-first-class-callable: ~S"
(it-sequential-each
      (("<?php function sq($x){return $x*$x;} $f=sq(...); echo $f(5);" "25")
       ("<?php function sub($a,$b){return $a-$b;} $f=sub(...); echo $f(10,3);"
        "7")
       ("<?php function dbl($x){return $x*2;} $f=dbl(...); echo array_sum(array_map($f,[1,2,3]));"
        "12")
       ("<?php $f=strlen(...); echo $f('hello');" "5")
       ("<?php function sq($x){return $x*$x;} echo array_sum(array_map(sq(...),[1,2,3]));"
        "14")
       ("<?php function sq($x){return $x*$x;} echo sq(5);" "25")
       ("<?php function s(...$n){return array_sum($n);} $a=[1,2]; echo s(...$a);"
        "3"))
      "php-e2e-first-class-callable: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/parser-e2e-test.lisp:32 "php-e2e-echo-no-trailing-newline-multi"
(it-sequential
    "php-e2e-echo-no-trailing-newline-multi"
    (expect
      (%php-run-capture "<?php echo 'a'; echo 'b'; echo 'c';")
      :to-equal
      "abc"))

;; t/parser-e2e-test.lisp:38 "php-e2e-echo-no-trailing-newline-multiarg"
(it-sequential
    "php-e2e-echo-no-trailing-newline-multiarg"
    (expect (%php-run-capture "<?php echo 'x', 'y', 'z';") :to-equal "xyz"))

;; t/parser-e2e-test.lisp:41 "php-e2e-echo-interior-newline-preserved"
(it-sequential
    "php-e2e-echo-interior-newline-preserved"
    (expect
      (%php-run-capture "<?php echo \"p\\n\"; echo 'q';")
      :to-equal
      (format nil "p~%q")))

;; t/parser-e2e-test.lisp:47 "php-e2e-inline-html-verbatim"
(it-sequential
    "php-e2e-inline-html-verbatim"
    (expect
      (%php-run-capture "<?php echo 1; ?>hello<?php echo 2;")
      :to-equal
      "1hello2"))

;; t/parser-e2e-test.lisp:117 "php-e2e-heredoc-nowdoc"
(it-sequential
    "php-e2e-heredoc-nowdoc"
    (expect
      (%php-run-capture (format nil "<?php $n=\"Bob\"; echo <<<EOT~%Hi $n~%EOT;~%"))
      :to-equal
      "Hi Bob")
    (expect
      (%php-run-capture
        (format nil "<?php $a=\"X\";$b=\"Y\"; echo <<<EOT~%$a-$b~%EOT;~%"))
      :to-equal
      "X-Y")
    (expect
      (%php-run-capture
        (format nil "<?php $n=\"Bob\"; echo <<<EOT~%Hi {$n}!~%EOT;~%"))
      :to-equal
      "Hi Bob!")
    (expect
      (%php-run-capture
        (format nil "<?php $n=\"Z\"; echo <<<EOT~%line1~%val=$n~%EOT;~%"))
      :to-equal
      (format nil "line1~%val=Z"))
    (expect
      (%php-run-capture (format nil "<?php $n=\"Q\"; echo <<<\"EOT\"~%v=$n~%EOT;~%"))
      :to-equal
      "v=Q")
    (expect
      (%php-run-capture (format nil "<?php $n=\"Bob\"; echo <<<'EOT'~%Hi $n~%EOT;~%"))
      :to-equal
      "Hi $n"))

;; t/parser-e2e-test.lisp:221 "php-e2e-logical-short-circuit"
(it-sequential
    "php-e2e-logical-short-circuit"
    (expect
      (%php-run-capture
        "<?php function boom(){ return 1/0; } $x=false; echo ($x && boom()) ? 'T':'F';")
      :to-equal
      "F"))

;; t/parser-e2e-test.lisp:337 "php-parser-cli-compile-path-for-php-files"
(it-sequential
    "php-parser-cli-compile-path-for-php-files"
    (let* ((tmp-dir (uiop:temporary-directory))
           (input (merge-pathnames "cl-cc-php-compile-gap.php" tmp-dir))
           (output (merge-pathnames "cl-cc-php-compile-gap" tmp-dir)))
      (with-open-file (stream input :direction :output :if-exists :supersede)
        (write-line "<?php echo match($x) { 1 => 'one', default => 'other' };" stream))
      (let ((result (cl-cc/pipeline::compile-file-to-native input :output-file output)))
        (expect result :to-equal output)
        (expect (probe-file output) :to-be-truthy))))

;; t/parser-e2e-test.lisp:359 "php-compile-expression-operators"
(it-sequential
    "php-compile-expression-operators"
    (let ((forms
            (cl-cc/php:parse-php-source
             "<?php $a = 2 ** 3 ** 2; $b = 7 % 4; $c = 1 << 3; $d = 8 >> 1; $e = 1 <=> 2; $f = 6 & 3; $g = 6 ^ 3; $h = 4 | 1; $i = ~1; $j = 1 + 2 . 3;")))
      (expect (= 1 (length forms)) :to-be-truthy)
      (expect (cl-cc/ast:ast-let-p (first forms)) :to-be-truthy)))

;; t/parser-e2e-test.lisp:459 "goto's target label statement is not yet supported, but now reports a
clear diagnostic instead of a confusing generic parse error"
(it-sequential
    "goto's target label statement is not yet supported, but now reports a
clear diagnostic instead of a confusing generic parse error"
    ;; goto/label had zero test coverage of any kind before this. PHP-PARSE-STATEMENT
    ;; (src/parser-class.lisp) dispatches only on :T-KEYWORD tokens via *PHP-STMT-PARSERS*, so
    ;; `goto label;` itself parsed fine (:goto IS a registered keyword parser) but the
    ;; corresponding `label:` statement fell through to the generic expression-statement
    ;; parser and failed with "unexpected token T-COLON in expression" — technically correct,
    ;; but gives no indication the user was actually attempting a label statement.
    ;;
    ;; A full fix is a real feature, not a bounded one: it needs a label-statement AST
    ;; representation, block-level lowering that recognizes labels and switches a plain
    ;; statement block to a CL TAGBODY (today only loops/switches build one internally, for
    ;; their own generated break/continue tags — see %PHP-MAKE-TAGBODY in
    ;; parser-stmt-lowering.lisp), and correctly modeling PHP's own goto-scoping restrictions
    ;; (no jumping into a loop/switch/variable scope from outside it) — rushing that risks a
    ;; silently-wrong jump target, worse than a clean error. What IS bounded and safe: PHP-
    ;; PARSE-STATEMENT now recognizes the "T-IDENT immediately followed by T-COLON" shape
    ;; explicitly and reports it through the existing %PHP-UNSUPPORTED mechanism (the same one
    ;; every other not-yet-supported parse form in this codebase uses), so the error at least
    ;; names what construct triggered it.
    (let ((condition
            (handler-case
                (progn (%php-run-capture "<?php echo 'a'; goto skip; echo 'b'; skip: echo 'c';")
                       nil)
              (error (e) e))))
      (expect condition :to-be-truthy)
      (expect (search "labeled statement" (princ-to-string condition)) :to-be-truthy)))

;; t/parser-e2e-test.lisp:487 "prefix ++ on a non-assignable operand reports a clear diagnostic, not an
internal arity error"
(it-sequential
    "prefix ++ on a non-assignable operand reports a clear diagnostic, not an
internal arity error"
    ;; %PHP-UNSUPPORTED takes exactly one argument (MESSAGE), but this call site passed an
    ;; extra, unused OPERAND argument — so instead of the intended "Unsupported PHP parse
    ;; form: PHP prefix ++ is only supported on a $variable, array element, or property"
    ;; message, every prefix ++/-- on a non-assignable operand raised a confusing internal
    ;; SB-INT:SIMPLE-PROGRAM-ERROR "invalid number of arguments: 2" instead.
    (signals simple-error
      (%php-run-capture "<?php ++(1+1);")))

;; t/parser-php85-language-test.lisp:28 "A parsed pipe chain can execute PHP first-class callable RHS expressions."
(it-sequential
    "A parsed pipe chain can execute PHP first-class callable RHS expressions."
    (expect
      (%php-run-capture "<?php echo \"  HI  \" |> trim(...) |> strtolower(...);")
      :to-equal
      "hi"))

;; t/parser-php85-language-test.lisp:46 "The PHP 8.5 array_first() and array_last() builtins execute from PHP source."
(it-sequential
    "The PHP 8.5 array_first() and array_last() builtins execute from PHP source."
    (expect
      (%php-run-capture
        "<?php $a=['a'=>10,'b'=>20]; echo array_first($a).':'.array_last($a);")
      :to-equal
      "10:20"))

;; t/parser-php85-language-test.lisp:57 "The PHP 8.5 grapheme_levenshtein() builtin executes from PHP source."
(it-sequential
    "The PHP 8.5 grapheme_levenshtein() builtin executes from PHP source."
    (expect
      (%php-run-capture "<?php echo grapheme_levenshtein('kitten','sitting');")
      :to-equal
      "3"))

;; t/parser-php85-language-test.lisp:70 "The PHP 8.5 locale_is_right_to_left() builtin executes from PHP source."
(it-sequential
    "The PHP 8.5 locale_is_right_to_left() builtin executes from PHP source."
    (expect
      (%php-run-capture
        "<?php echo (locale_is_right_to_left('he_IL') ? 'rtl' : 'ltr') . ':' . (locale_is_right_to_left('en_US') ? 'rtl' : 'ltr');")
      :to-equal
      "rtl:ltr"))

;; t/parser-php85-language-test.lisp:77 "The PHP 8.5 Locale::isRightToLeft() static method lowers to the runtime helper."
(it-sequential
    "The PHP 8.5 Locale::isRightToLeft() static method lowers to the runtime helper."
    (expect
      (%php-run-capture
        "<?php echo (Locale::isRightToLeft('ur_PK') ? 'rtl' : 'ltr') . ':' . (Locale::isRightToLeft('fr_FR') ? 'rtl' : 'ltr');")
      :to-equal
      "rtl:ltr"))

;; t/parser-php85-language-test.lisp:92 "PHP 8.5 build metadata constants are available to parsed PHP code."
(it-sequential
    "PHP 8.5 build metadata constants are available to parsed PHP code."
    (expect
      (%php-run-capture
        "<?php echo PHP_BUILD_PROVIDER . ':' . (PHP_BUILD_DATE === '' ? 'empty' : 'date');")
      :to-equal
      "cl-cc:date"))

;; t/parser-php85-language-test.lisp:117 "Discarding a #[NoDiscard] function result emits E_USER_WARNING."
(it-sequential
    "Discarding a #[NoDiscard] function result emits E_USER_WARNING."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo $errno . ':' . (str_contains($errstr, 'important()') ? 'name' : 'missing') . ':' . (str_contains($errstr, 'must use it') ? 'msg' : 'missing') . ':'; return true; } set_error_handler('h', E_USER_WARNING); #[NoDiscard('must use it')] function important() { echo '7'; return 7; } important(); restore_error_handler();")
      :to-equal
      "512:name:msg:7"))

;; t/parser-php85-language-test.lisp:124 "Using a #[NoDiscard] function result does not emit a warning."
(it-sequential
    "Using a #[NoDiscard] function result does not emit a warning."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_USER_WARNING); #[NoDiscard] function important(){ return 7; } $x = important(); echo $x; restore_error_handler();")
      :to-equal
      "7"))

;; t/parser-php85-language-test.lisp:131 "Casting a #[NoDiscard] function result to void suppresses the warning."
(it-sequential
    "Casting a #[NoDiscard] function result to void suppresses the warning."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_USER_WARNING); #[NoDiscard] function important(){ echo 'called'; return 7; } (void) important(); restore_error_handler();")
      :to-equal
      "called"))

;; t/parser-php85-language-test.lisp:138 "Discarding a #[NoDiscard] method result emits E_USER_WARNING."
(it-sequential
    "Discarding a #[NoDiscard] method result emits E_USER_WARNING."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo $errno . ':' . (str_contains($errstr, 'label()') ? 'name' : 'missing') . ':' . (str_contains($errstr, 'must use method') ? 'msg' : 'missing') . ':'; return true; } set_error_handler('h', E_USER_WARNING); class Box { #[NoDiscard('must use method')] public function label(){ echo 'm'; return 'm'; } } $box = new Box(); $box->label(); restore_error_handler();")
      :to-equal
      "512:name:msg:m"))

;; t/parser-php85-language-test.lisp:145 "Using a #[NoDiscard] method result does not emit a warning."
(it-sequential
    "Using a #[NoDiscard] method result does not emit a warning."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_USER_WARNING); class Box { #[NoDiscard] public function label(){ return 'm'; } } $box = new Box(); $x = $box->label(); echo $x; restore_error_handler();")
      :to-equal
      "m"))

;; t/parser-php85-language-test.lisp:152 "Casting a #[NoDiscard] method result to void suppresses the warning."
(it-sequential
    "Casting a #[NoDiscard] method result to void suppresses the warning."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_USER_WARNING); class Box { #[NoDiscard] public function label(){ echo 'm'; return 'm'; } } $box = new Box(); (void) $box->label(); restore_error_handler();")
      :to-equal
      "m"))

;; t/parser-php85-language-test.lisp:159 "Discarding a #[NoDiscard] static method result emits E_USER_WARNING."
(it-sequential
    "Discarding a #[NoDiscard] static method result emits E_USER_WARNING."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo $errno . ':' . (str_contains($errstr, 'label()') ? 'name' : 'missing') . ':'; return true; } set_error_handler('h', E_USER_WARNING); class Box { #[NoDiscard] public static function label(){ echo 's'; return 's'; } } Box::label(); restore_error_handler();")
      :to-equal
      "512:name:s"))

;; t/parser-php85-language-test.lisp:166 "PHP top-level const declarations define readable constants."
(it-sequential
    "PHP top-level const declarations define readable constants."
    (expect
      (%php-run-capture "<?php const ANSWER = 42; echo ANSWER;")
      :to-equal
      "42"))

;; t/parser-php85-language-test.lisp:239 "The PHP 8.5 (void) statement still evaluates the discarded expression."
(it-sequential
    "The PHP 8.5 (void) statement still evaluates the discarded expression."
    (expect (%php-run-capture "<?php $x=0; (void)($x=1); echo $x;") :to-equal "1"))

;; t/parser-property-hooks-e2e-test.lisp:14 "public private(set) allows external reads of the property"
(it-sequential "public private(set) allows external reads of the property"
  (expect (%php-run-capture
           "<?php class Point { public private(set) int $x = 5; } $p = new Point(); echo $p->x;")
          :to-equal "5"))

;; t/parser-property-hooks-e2e-test.lisp:23 "a function typed with an intersection type accepts a matching object"
(it-sequential "a function typed with an intersection type accepts a matching object"
  (expect (%php-run-capture
           "<?php interface Countable2 { function count2(); } interface Stringable2 { function __toString(); } class Both implements Countable2, Stringable2 { function count2() { return 3; } function __toString() { return 's'; } } function f(Countable2&Stringable2 $x) { return $x->count2(); } $o = new Both(); echo f($o);")
          :to-equal "3"))

;; t/runtime-builtins-array-e2e-test.lisp:63 "php-e2e-array-column-basic-and-index: ~S"
(it-sequential-each
      (("<?php $rows=[['id'=>10,'name'=>'Ada'],['id'=>20,'name'=>'Linus']]; $r=array_column($rows,'name','id'); echo $r[10].','.$r[20];"
        "Ada,Linus")
       ("<?php $rows=[['name'=>'Ada'],['id'=>20,'name'=>'Linus']]; $r=array_column($rows,'name','id'); echo $r[0].','.$r[20];"
        "Ada,Linus"))
      "php-e2e-array-column-basic-and-index: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-array-e2e-test.lisp:85 "php-e2e-array-keys-filters-values: ~S"
(it-sequential-each
      (("<?php $r=array_keys(['a'=>1,'b'=>'1','c'=>2], 1); echo implode(',', $r);"
        "a,b")
       ("<?php $r=array_keys(['a'=>1,'b'=>'1','c'=>2], 1, true); echo implode(',', $r);"
        "a")
       ("<?php $r=array_keys(['x'=>null,'y'=>0], null, true); echo implode(',', $r);"
        "x"))
      "php-e2e-array-keys-filters-values: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-array-e2e-test.lisp:95 "php-e2e-array-search-strict-and-miss: ~S"
(it-sequential-each
      (("<?php $a=['s'=>'1','i'=>1]; echo array_search(1,$a).':'.array_search(1,$a,true).':'.(array_search(2,$a)===false?'false':'bad');"
        "s:i:false")
       ("<?php $a=[0=>'needle']; echo array_search('needle',$a)===0?'zero':'bad';"
        "zero"))
      "php-e2e-array-search-strict-and-miss: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-array-e2e-test.lisp:180 "php-e2e-array-filter-mode-flags: ~S"
(it-sequential-each
      (("<?php $f=array_filter(['a'=>1,'b'=>2,'c'=>3], fn($k)=>$k!='b', ARRAY_FILTER_USE_KEY); echo implode(',', array_keys($f));"
        "a,c")
       ("<?php $f=array_filter(['a'=>1,'b'=>2,'c'=>3], fn($v,$k)=>$v>1 && $k!='a', ARRAY_FILTER_USE_BOTH); echo implode(',', array_keys($f));"
        "b,c")
       ("<?php $f=array_filter(['x'=>0,'y'=>2], null, ARRAY_FILTER_USE_KEY); echo implode(',', $f);"
        "2"))
      "php-e2e-array-filter-mode-flags: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-array-e2e-test.lisp:190 "php-e2e-array-pad-key-policy: ~S"
(it-sequential-each
      (("<?php $r=array_pad(['x'=>1,4=>2],4,0); $o=''; foreach($r as $k=>$v){ $o=$o.$k.':'.$v.'|'; } echo $o;"
        "x:1|0:2|1:0|2:0|")
       ("<?php $r=array_pad(['x'=>1,4=>2],-4,0); $o=''; foreach($r as $k=>$v){ $o=$o.$k.':'.$v.'|'; } echo $o;"
        "0:0|1:0|x:1|2:2|")
       ("<?php $r=array_pad(['x'=>1,4=>2],2,0); $o=''; foreach($r as $k=>$v){ $o=$o.$k.':'.$v.'|'; } echo $o;"
        "x:1|0:2|"))
      "php-e2e-array-pad-key-policy: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-array-e2e-test.lisp:200 "php-e2e-array-append: ~S"
(it-sequential-each
      (("<?php $a=[1,2]; $a[]=3; echo count($a);" "3")
       ("<?php $a=[1,2]; $a[]=3; echo $a[2];" "3")
       ("<?php $a=[]; $a[]='x'; $a[]='y'; echo $a[0].$a[1];" "xy")
       ("<?php $a=[]; for($i=0;$i<3;$i++){ $a[]=$i*10; } echo $a[0].$a[1].$a[2];"
        "01020")
       ("<?php $a=['k'=>1]; $a[]=2; echo $a['k'].$a[0];" "12"))
      "php-e2e-array-append: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-array-e2e-test.lisp:210 "php-e2e-array-subscript-set-still-works: ~S"
(it-sequential-each
      (("<?php $a=[1]; $a[0]=9; echo $a[0];" "9")
       ("<?php $a=[]; $a['x']=5; echo $a['x'];" "5"))
      "php-e2e-array-subscript-set-still-works: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-array-e2e-test.lisp:224 "php-e2e-array-spread: ~S"
(it-sequential-each
      (("<?php $a=[1,2]; $b=[...$a,3]; echo array_sum($b);" "6")
       ("<?php $a=[2,3]; $b=[1,...$a,4]; echo implode(',',$b);" "1,2,3,4")
       ("<?php $a=[1]; $b=[2]; $c=[...$a,...$b]; echo count($c);" "2")
       ("<?php $a=[5,6]; $b=[...$a]; echo array_sum($b);" "11")
       ("<?php $a=['x'=>1]; $b=[...$a,'y'=>2]; echo $b['x'].$b['y'];" "12")
       ("<?php $a=[5=>1, 9=>2]; $b=[...$a]; echo $b[0].$b[1];" "12")
       ("<?php echo array_sum([1,2,3]);" "6")
       ("<?php function s(...$n){return array_sum($n);} $a=[1,2,3]; echo s(...$a);"
        "6"))
      "php-e2e-array-spread: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-array-e2e-test.lisp:237 "php-e2e-array-union: ~S"
(it-sequential-each
      (("<?php $a=[1,2]+[3,4,5]; echo count($a);" "3")
       ("<?php $a=[1,2]+[3,4,5]; echo implode(',',$a);" "1,2,5")
       ("<?php $a=['x'=>1]+['x'=>9,'y'=>2]; echo $a['x'].$a['y'];" "12")
       ("<?php $a=[1=>'a']+[1=>'b',2=>'c']; echo $a[1].$a[2];" "ac")
       ("<?php $a=[]+[1,2]; echo count($a);" "2")
       ("<?php echo 2+3;" "5")
       ("<?php echo '5'+3;" "8")
       ("<?php echo count(array_merge([1,2],[3,4]));" "4"))
      "php-e2e-array-union: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-array-e2e-test.lisp:249 "php-e2e-intdiv-fdiv-array-is-list: ~S"
(it-sequential-each
      (("<?php echo intdiv(7,2);" "3")
       ("<?php echo intdiv(-7,2);" "-3")
       ("<?php echo intdiv('10','3');" "3")
       ("<?php echo fdiv(10,4);" "2.5")
       ("<?php echo fdiv(6,2);" "3")
       ("<?php echo array_is_list([1,2,3])?'y':'n';" "y")
       ("<?php echo array_is_list([1=>'a',0=>'b'])?'y':'n';" "n")
       ("<?php echo array_is_list(['x'=>1])?'y':'n';" "n")
       ("<?php echo array_is_list([])?'y':'n';" "y"))
      "php-e2e-intdiv-fdiv-array-is-list: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-array-e2e-test.lisp:262 "php-e2e-usort-in-place: ~S"
(it-sequential-each
      (("<?php $a=[3,1,2]; usort($a, fn($x,$y)=>$x-$y); echo implode(',',$a);"
        "1,2,3")
       ("<?php $a=[3,1,2]; usort($a, fn($x,$y)=>$y-$x); echo implode(',',$a);"
        "3,2,1")
       ("<?php function cmp($a,$b){return $a-$b;} $c=[5,2,8,1]; usort($c,'cmp'); echo implode(',',$c);"
        "1,2,5,8")
       ("<?php $d=['banana','apple','cherry']; usort($d,'strcmp'); echo implode(',',$d);"
        "apple,banana,cherry")
       ("<?php $e=['b'=>2,'a'=>1,'c'=>3]; uasort($e, fn($x,$y)=>$x-$y); echo implode(',',array_keys($e));"
        "a,b,c")
       ("<?php $f=['banana'=>1,'apple'=>2]; uksort($f,'strcmp'); echo implode(',',array_keys($f));"
        "apple,banana")
       ("<?php $g=[['k'=>1,'v'=>'a'],['k'=>1,'v'=>'b'],['k'=>0,'v'=>'c']]; usort($g, fn($x,$y)=>$x['k']-$y['k']); echo $g[0]['v'].$g[1]['v'].$g[2]['v'];"
        "cab"))
      "php-e2e-usort-in-place: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-array-e2e-test.lisp:280 "php-e2e-array-multisort-syncs-arrays-and-flags: ~S"
(it-sequential-each
      (("<?php $a=[2,2,1]; $b=['b','a','c']; array_multisort($a, SORT_ASC, SORT_NUMERIC, $b, SORT_ASC, SORT_STRING); echo implode(',',$a).'|'.implode(',',$b);"
        "1,2,2|c,a,b")
       ("<?php $a=[1,10,2]; $b=['a','b','c']; array_multisort($a, SORT_DESC, SORT_NUMERIC, $b); echo implode(',',$a).'|'.implode(',',$b);"
        "10,2,1|b,c,a")
       ("<?php $a=['first'=>2,'third'=>1]; $b=[20,10]; array_multisort($a, $b); echo implode(',',array_keys($a)).'|'.implode(',',array_keys($b));"
        "third,first|0,1"))
      "php-e2e-array-multisort-syncs-arrays-and-flags: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-array-e2e-test.lisp:290 "php-e2e-sort-builtins-honor-sort-flags: ~S"
(it-sequential-each
      (("<?php $a=['10','2','1']; sort($a, SORT_NUMERIC); echo implode(',',$a);"
        "1,2,10")
       ("<?php $a=['10','2','1']; sort($a, SORT_STRING); echo implode(',',$a);"
        "1,10,2")
       ("<?php $a=['10','2','1']; rsort($a, SORT_NUMERIC); echo implode(',',$a); $b=['10','2','1']; rsort($b, SORT_STRING); echo '|'.implode(',',$b);"
        "10,2,1|2,10,1")
       ("<?php $a=['a'=>'10','b'=>'2','c'=>'1']; asort($a, SORT_NUMERIC); echo implode(',',array_keys($a)); $b=['a'=>'10','b'=>'2','c'=>'1']; arsort($b, SORT_STRING); echo '|'.implode(',',array_keys($b));"
        "c,b,a|b,a,c")
       ("<?php $a=[10=>'a',2=>'b',1=>'c']; ksort($a, SORT_STRING); echo implode(',',array_keys($a)); $b=[10=>'a',2=>'b',1=>'c']; krsort($b, SORT_STRING); echo '|'.implode(',',array_keys($b));"
        "1,10,2|2,10,1"))
      "php-e2e-sort-builtins-honor-sort-flags: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-array-e2e-test.lisp:304 "php-e2e-natsort: ~S"
(it-sequential-each
      (("<?php $a=['img10','img2','img1']; natsort($a); echo implode(',',$a);"
        "img1,img2,img10")
       ("<?php $a=['file10.txt','file1.txt','file2.txt']; natsort($a); echo implode(',',$a);"
        "file1.txt,file2.txt,file10.txt")
       ("<?php $a=[3=>'a10',1=>'a2',2=>'a1']; natsort($a); echo implode(',',array_keys($a));"
        "2,1,3")
       ("<?php $a=['IMG10','img2','Img1']; natcasesort($a); echo implode(',',$a);"
        "Img1,img2,IMG10")
       ("<?php echo strnatcmp('img2','img10');" "-1")
       ("<?php echo strnatcmp('img10','img2');" "1")
       ("<?php echo strnatcmp('abc','abc');" "0")
       ("<?php echo strnatcasecmp('IMG2','img10');" "-1"))
      "php-e2e-natsort: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-array-e2e-test.lisp:320 "php-e2e-array-merge-recursive: ~S"
(it-sequential-each
      (("<?php echo json_encode(array_merge_recursive(['a'=>[1]],['a'=>[2]]));"
        "{\"a\":[1,2]}")
       ("<?php echo json_encode(array_merge_recursive(['a'=>1],['a'=>2]));"
        "{\"a\":[1,2]}")
       ("<?php echo json_encode(array_merge_recursive(['a'=>[1]],['a'=>2]));"
        "{\"a\":[1,2]}")
       ("<?php echo json_encode(array_merge_recursive(['a'=>['x'=>1]],['a'=>['x'=>2]]));"
        "{\"a\":{\"x\":[1,2]}}")
       ("<?php echo json_encode(array_merge_recursive([1,2],[3,4]));"
        "[1,2,3,4]")
       ("<?php echo json_encode(array_merge_recursive(['a'=>1],['b'=>2]));"
        "{\"a\":1,\"b\":2}")
       ("<?php echo json_encode(array_merge_recursive(['a'=>[1]],['a'=>[2]],['a'=>[3]]));"
        "{\"a\":[1,2,3]}"))
      "php-e2e-array-merge-recursive: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-array-e2e-test.lisp:8 "php-e2e-array-map-closure-callback"
(it-sequential
    "php-e2e-array-map-closure-callback"
    (expect
      (%php-run-capture
        "<?php $f=function($x){ return $x*10; }; $a=[1,2,3]; $r=array_map($f,$a); echo $r[0].','.$r[1].','.$r[2];")
      :to-equal
      "10,20,30"))

;; t/runtime-builtins-array-e2e-test.lisp:15 "php-e2e-array-map-preserves-single-array-keys"
(it-sequential
    "php-e2e-array-map-preserves-single-array-keys"
    (expect
      (%php-run-capture
        "<?php $r=array_map(null,['x'=>7,'y'=>8]); echo $r['x'].','.$r['y'];")
      :to-equal
      "7,8"))

;; t/runtime-builtins-array-e2e-test.lisp:22 "php-e2e-array-map-multiple-arrays"
(it-sequential
    "php-e2e-array-map-multiple-arrays"
    (expect
      (%php-run-capture
        "<?php $f=function($a,$b){ return $a+$b; }; $r=array_map($f,[1,2,3],[10,20,30]); echo $r[0].','.$r[1].','.$r[2];")
      :to-equal
      "11,22,33"))

;; t/runtime-builtins-array-e2e-test.lisp:29 "php-e2e-array-map-multiple-arrays-pads-missing-with-null"
(it-sequential
    "php-e2e-array-map-multiple-arrays-pads-missing-with-null"
    (expect
      (%php-run-capture
        "<?php $f=function($a,$b){ return $a.($b===null?'null':$b); }; $r=array_map($f,[1,2],['a']); echo $r[0].','.$r[1];")
      :to-equal
      "1a,2null"))

;; t/runtime-builtins-array-e2e-test.lisp:36 "php-e2e-array-map-null-callback-zips"
(it-sequential
    "php-e2e-array-map-null-callback-zips"
    (expect
      (%php-run-capture
        "<?php $r=array_map(null,[1,2],['a','b']); echo $r[0][0].$r[0][1].','.$r[1][0].$r[1][1];")
      :to-equal
      "1a,2b"))

;; t/runtime-builtins-array-e2e-test.lisp:43 "php-e2e-array-map-null-callback-pads-missing-with-null"
(it-sequential
    "php-e2e-array-map-null-callback-pads-missing-with-null"
    (expect
      (%php-run-capture "<?php $r=array_map(null,[1,2],['a']); echo json_encode($r);")
      :to-equal
      "[[1,\"a\"],[2,null]]"))

;; t/runtime-builtins-array-e2e-test.lisp:49 "php-e2e-array-slice-preserves-string-keys"
(it-sequential
    "php-e2e-array-slice-preserves-string-keys"
    (expect
      (%php-run-capture
        "<?php $r=array_slice(['x'=>1,5=>2,'y'=>3],0); echo $r['x'].','.$r[0].','.$r['y'];")
      :to-equal
      "1,2,3"))

;; t/runtime-builtins-array-e2e-test.lisp:56 "php-e2e-array-reverse-preserves-string-keys"
(it-sequential
    "php-e2e-array-reverse-preserves-string-keys"
    (expect
      (%php-run-capture
        "<?php $r=array_reverse(['a'=>1,4=>2,'b'=>3]); echo $r['b'].','.$r[0].','.$r['a'];")
      :to-equal
      "3,2,1"))

;; t/runtime-builtins-array-e2e-test.lisp:71 "php-e2e-array-column-skips-missing-column"
(it-sequential
    "php-e2e-array-column-skips-missing-column"
    (expect
      (%php-run-capture
        "<?php $rows=[['name'=>'Ada'],['id'=>20],['name'=>'Grace']]; $r=array_column($rows,'name'); echo count($r).':'.$r[0].':'.$r[1];")
      :to-equal
      "2:Ada:Grace"))

;; t/runtime-builtins-array-e2e-test.lisp:78 "php-e2e-array-column-null-column-returns-rows"
(it-sequential
    "php-e2e-array-column-null-column-returns-rows"
    (expect
      (%php-run-capture
        "<?php $rows=[['id'=>'a','name'=>'Ada'],['id'=>'b','name'=>'Bob']]; $r=array_column($rows,null,'id'); echo $r['a']['name'].','.$r['b']['name'];")
      :to-equal
      "Ada,Bob"))

;; t/runtime-builtins-array-e2e-test.lisp:103 "php-e2e-in-array-strict"
(it-sequential
    "php-e2e-in-array-strict"
    (expect
      (%php-run-capture
        "<?php $a=['1']; echo (in_array(1,$a)?'loose':'x').':'.(in_array(1,$a,true)?'bad':'strict');")
      :to-equal
      "loose:strict"))

;; t/runtime-builtins-array-e2e-test.lisp:110 "php-e2e-array-unique-compares-values-as-strings"
(it-sequential
    "php-e2e-array-unique-compares-values-as-strings"
    (expect
      (%php-run-capture
        "<?php echo json_encode(array_unique(['a'=>0,'b'=>'0','c'=>false,'d'=>null,'e'=>'']));")
      :to-equal
      "{\"a\":0,\"c\":false}"))

;; t/runtime-builtins-array-e2e-test.lisp:117 "php-e2e-array-count-values-counts-only-ints-and-strings"
(it-sequential
    "php-e2e-array-count-values-counts-only-ints-and-strings"
    (expect
      (%php-run-capture
        "<?php echo json_encode(array_count_values([1,'1',true,null,1.5,'x','x']));")
      :to-equal
      "{\"1\":2,\"x\":2}"))

;; t/runtime-builtins-array-e2e-test.lisp:124 "php-e2e-array-flip-skips-non-key-values"
(it-sequential
    "php-e2e-array-flip-skips-non-key-values"
    (expect
      (%php-run-capture
        "<?php echo json_encode(array_flip(['a'=>'x','b'=>1,'c'=>true,'d'=>null,'e'=>1.5]));")
      :to-equal
      "{\"x\":\"a\",\"1\":\"b\"}"))

;; t/runtime-builtins-array-e2e-test.lisp:131 "php-e2e-array-diff-compares-values-as-strings"
(it-sequential
    "php-e2e-array-diff-compares-values-as-strings"
    (expect
      (%php-run-capture
        "<?php echo json_encode(array_values(array_diff([0,'0','x'], ['0'])));")
      :to-equal
      "[\"x\"]"))

;; t/runtime-builtins-array-e2e-test.lisp:138 "php-e2e-array-intersect-compares-values-as-strings"
(it-sequential
    "php-e2e-array-intersect-compares-values-as-strings"
    (expect
      (%php-run-capture
        "<?php echo json_encode(array_values(array_intersect([0,'0','x'], ['0'])));")
      :to-equal
      "[0,\"0\"]"))

;; t/runtime-builtins-array-e2e-test.lisp:145 "php-e2e-array-diff-assoc-compares-values-as-strings"
(it-sequential
    "php-e2e-array-diff-assoc-compares-values-as-strings"
    (expect
      (%php-run-capture
        "<?php echo json_encode(array_diff_assoc(['a'=>0,'b'=>'x'], ['a'=>'0']));")
      :to-equal
      "{\"b\":\"x\"}"))

;; t/runtime-builtins-array-e2e-test.lisp:152 "php-e2e-array-intersect-assoc-compares-values-as-strings"
(it-sequential
    "php-e2e-array-intersect-assoc-compares-values-as-strings"
    (expect
      (%php-run-capture
        "<?php echo json_encode(array_intersect_assoc(['a'=>0,'b'=>'x'], ['a'=>'0','b'=>'y']));")
      :to-equal
      "{\"a\":0}"))

;; t/runtime-builtins-array-e2e-test.lisp:159 "php-e2e-array-diff-assoc-matches-explicit-null"
(it-sequential
    "php-e2e-array-diff-assoc-matches-explicit-null"
    (expect
      (%php-run-capture
        "<?php echo json_encode(array_diff_assoc(['a'=>null], ['a'=>null]));")
      :to-equal
      "[]"))

;; t/runtime-builtins-array-e2e-test.lisp:166 "php-e2e-array-intersect-assoc-distinguishes-missing-key-from-null"
(it-sequential
    "php-e2e-array-intersect-assoc-distinguishes-missing-key-from-null"
    (expect
      (%php-run-capture
        "<?php echo json_encode(array_intersect_assoc(['a'=>null], []));")
      :to-equal
      "[]"))

;; t/runtime-builtins-array-e2e-test.lisp:173 "php-e2e-array-filter-closure"
(it-sequential
    "php-e2e-array-filter-closure"
    (expect
      (%php-run-capture
        "<?php $f=array_filter([0,1,0,2], function($x){ return $x; }); $s=0; foreach($f as $v){ $s=$s+$v; } echo $s;")
      :to-equal
      "3"))

;; t/runtime-builtins-array-e2e-test.lisp:338 "php-e2e-array-walk-invokes-the-callback-with-value-and-key"
(it-sequential
    "php-e2e-array-walk-invokes-the-callback-with-value-and-key"
    (expect (%php-run-capture
             "<?php array_walk(['a'=>1,'b'=>2], function($v,$k){ echo $k,$v; });")
            :to-equal "a1b2"))

;; t/runtime-builtins-array-e2e-test.lisp:343 "array_walk/array_walk_recursive's by-ref callback parameter does not
mutate the array — the callback is invoked with the plain value, not a PHP
reference box, so any assignment inside a `function(&$value)` callback is
lost"
(it-sequential
    "array_walk/array_walk_recursive's by-ref callback parameter does not
mutate the array — the callback is invoked with the plain value, not a PHP
reference box, so any assignment inside a `function(&$value)` callback is
lost"
    ;; PHP's real array_walk/array_walk_recursive pass $value BY REFERENCE
    ;; (the callback signature is `function(&$value, $key)`), so a callback
    ;; that reassigns $value is documented to mutate the array in place.
    ;; %PHP-ARRAY-WALK/-RECURSIVE call the callback with the bare value
    ;; (`(funcall fn (cdr pair) (car pair))`, no %PHP-MAKE-REF box and no
    ;; writeback afterward), so this runtime's array is left unchanged.
    (expect (%php-run-capture
             "<?php $a=[1,2,3]; array_walk($a, function(&$v){ $v = $v * 10; }); echo implode(',', $a);")
            :to-equal "1,2,3")
    (expect (%php-run-capture
             "<?php $a=['x'=>[1,2]]; array_walk_recursive($a, function(&$v){ $v = $v * 10; }); echo implode(',', $a['x']);")
            :to-equal "1,2"))

;; t/runtime-builtins-io-cookie-session-php85-test.lisp:12 "PHP 8.5 setcookie() accepts partitioned cookies when secure is enabled."
(it-sequential
    "PHP 8.5 setcookie() accepts partitioned cookies when secure is enabled."
    (let ((cl-cc/php::*php-http-response-code* 200)
          (cl-cc/php::*php-http-headers* nil)
          (cl-cc/php::*php-output-started-p* nil))
      (expect
        (%php-run-capture
          "<?php
setcookie('chip', 'a b', ['secure' => true, 'partitioned' => true]);
echo json_encode(headers_list());")
        :to-equal
        "[\"Set-Cookie: chip=a+b; secure; Partitioned\"]")))

;; t/runtime-builtins-io-cookie-session-php85-test.lisp:24 "PHP 8.5 setcookie() option keys are case-insensitive."
(it-sequential
    "PHP 8.5 setcookie() option keys are case-insensitive."
    (let ((cl-cc/php::*php-http-response-code* 200)
          (cl-cc/php::*php-http-headers* nil)
          (cl-cc/php::*php-output-started-p* nil))
      (expect
        (%php-run-capture
          "<?php
setcookie('chip', 'v', ['Secure' => true, 'SameSite' => 'Strict', 'Partitioned' => true]);
echo json_encode(headers_list());")
        :to-equal
        "[\"Set-Cookie: chip=v; secure; SameSite=Strict; Partitioned\"]")))

;; t/runtime-builtins-io-cookie-session-php85-test.lisp:36 "PHP 8.5 setrawcookie() keeps raw values and supports partitioned cookies."
(it-sequential
    "PHP 8.5 setrawcookie() keeps raw values and supports partitioned cookies."
    (let ((cl-cc/php::*php-http-response-code* 200)
          (cl-cc/php::*php-http-headers* nil)
          (cl-cc/php::*php-output-started-p* nil))
      (expect
        (%php-run-capture
          "<?php
setrawcookie('raw', 'a b', ['secure' => true, 'samesite' => 'None', 'partitioned' => true]);
echo json_encode(headers_list());")
        :to-equal
        "[\"Set-Cookie: raw=a b; secure; SameSite=None; Partitioned\"]")))

;; t/runtime-builtins-io-cookie-session-php85-test.lisp:94 "PHP 8.5 session cookie params expose and emit partitioned cookies."
(it-sequential
    "PHP 8.5 session cookie params expose and emit partitioned cookies."
    (let ((cl-cc/php::*php-http-response-code* 200)
          (cl-cc/php::*php-http-headers* nil)
          (cl-cc/php::*php-output-started-p* nil)
          (cl-cc/php::*php-session-cookie-params* nil)
          (cl-cc/php::*php-session-id* "")
          (cl-cc/php::*php-session-name* "PHPSESSID")
          (cl-cc/php::*php-session-active-p* nil))
      (expect
        (%php-run-capture
          "<?php
$registered = function_exists('session_set_cookie_params') && function_exists('session_get_cookie_params') && function_exists('session_start') ? 'Y' : 'N';
session_id('12345');
session_set_cookie_params(['secure' => true, 'partitioned' => true]);
$params = session_get_cookie_params();
$partitioned = $params['partitioned'] ? 'true' : 'false';
session_start();
echo $registered . ':' . $partitioned . ':' . json_encode(headers_list());")
        :to-equal
        "Y:true:[\"Set-Cookie: PHPSESSID=12345; path=/; secure; Partitioned\"]")))

;; t/runtime-builtins-io-cookie-session-php85-test.lisp:115 "PHP 8.5 mail() is registered and accepts mail requests in the CLI model."
(it-sequential
    "PHP 8.5 mail() is registered and accepts mail requests in the CLI model."
    (expect
      (%php-run-capture
        "<?php
$registered = function_exists('mail') ? 'Y' : 'N';
$result = mail('to@example.com', 'subject', 'message', ['X-Test: value'], '-f bounce@example.com');
echo $registered . ':' . ($result ? 'true' : 'false');")
      :to-equal
      "Y:true"))

;; t/runtime-builtins-io-cookie-session-php85-test.lisp:125 "PHP 8.5 session cookie param option keys are case-insensitive."
(it-sequential
    "PHP 8.5 session cookie param option keys are case-insensitive."
    (let ((cl-cc/php::*php-session-cookie-params* nil)
          (cl-cc/php::*php-session-active-p* nil))
      (expect
        (%php-run-capture
          "<?php
session_set_cookie_params(['Secure' => true, 'Partitioned' => true]);
$params = session_get_cookie_params();
echo ($params['secure'] ? 'true' : 'false') . ':' . ($params['partitioned'] ? 'true' : 'false');")
        :to-equal
        "true:true")))

;; t/runtime-builtins-io-cookie-session-php85-test.lisp:147 "PHP 8.5 session_start() warns and fails when cookie_partitioned lacks cookie_secure."
(it-sequential
    "PHP 8.5 session_start() warns and fails when cookie_partitioned lacks cookie_secure."
    (let ((cl-cc/php::*php-http-response-code* 200)
          (cl-cc/php::*php-http-headers* nil)
          (cl-cc/php::*php-output-started-p* nil)
          (cl-cc/php::*php-session-cookie-params* nil)
          (cl-cc/php::*php-session-id* "")
          (cl-cc/php::*php-session-name* "PHPSESSID")
          (cl-cc/php::*php-session-active-p* nil)
          (cl-cc/php::*php-error-handler-stack* nil))
      (expect
        (%php-run-capture
          "<?php
function php85_session_warning($errno, $errstr, $file, $line) { echo $errno . ':'; return true; }
set_error_handler('php85_session_warning', E_WARNING);
session_id('12345');
session_set_cookie_params(['partitioned' => true]);
$ok = session_start();
restore_error_handler();
echo ($ok ? 'true' : 'false') . ':' . json_encode(headers_list());")
        :to-equal
        "2:false:[]")))

;; t/runtime-builtins-io-dom-php85-test.lisp:15 "PHP 8.5 Dom\\Element method additions execute from PHP source."
(it-sequential
    "PHP 8.5 Dom\\Element method additions execute from PHP source."
    (expect
      (%php-run-capture
        "<?php
$e = new Dom\\Element('div');
$e->insertAdjacentHTML('beforeend', '<span class=\"a\"></span>');
$items = $e->getElementsByClassName('a');
echo get_class($items) . ':' .
     (is_null($e->insertAdjacentHTML('afterbegin', '<b></b>')) ? 'Y' : 'N');
")
      :to-equal
      "Dom\\HTMLCollection:Y"))

;; t/runtime-builtins-io-dom-php85-test.lisp:28 "PHP 8.5 Dom\\Element exposes outerHTML and children properties from PHP source."
(it-sequential
    "PHP 8.5 Dom\\Element exposes outerHTML and children properties from PHP source."
    (expect
      (%php-run-capture
        "<?php
$e = new Dom\\Element('section');
echo $e->outerHTML . ':' . get_class($e->children) . ':' .
     (property_exists($e, 'outerHTML') ? 'Y' : 'N') . ':' .
     (property_exists($e, 'children') ? 'Y' : 'N') . ':' .
     (interface_exists('Dom\\\\ParentNode') ? 'Y' : 'N') . ':' .
     (class_exists('Dom\\\\HTMLCollection') ? 'Y' : 'N');
")
      :to-equal
      "<section></section>:Dom\\HTMLCollection:Y:Y:Y:Y"))

;; t/runtime-builtins-io-dom-php85-test.lisp:51 "PHP 8.5 Dom\\HTMLDocument::getElementsByName executes from PHP source."
(it-sequential
    "PHP 8.5 Dom\\HTMLDocument::getElementsByName executes from PHP source."
    (expect
      (%php-run-capture
        "<?php
$doc = new Dom\\HTMLDocument('<form><input name=\"token\"></form>');
$items = $doc->getElementsByName('token');
echo get_class($doc) . ':' . get_class($items) . ':' .
     (method_exists($doc, 'getElementsByName') ? 'Y' : 'N');
")
      :to-equal
      "Dom\\HTMLDocument:Dom\\HTMLCollection:Y"))

;; t/runtime-builtins-io-dom-php85-test.lisp:70 "PHP 8.5 Dom\\ParentNode children property is exposed on Dom\\HTMLDocument."
(it-sequential
    "PHP 8.5 Dom\\ParentNode children property is exposed on Dom\\HTMLDocument."
    (expect
      (%php-run-capture
        "<?php
$doc = new Dom\\HTMLDocument('<main></main>');
echo get_class($doc->children) . ':' .
     (property_exists($doc, 'children') ? 'Y' : 'N');
")
      :to-equal
      "Dom\\HTMLCollection:Y"))

;; t/runtime-builtins-io-objects-php85-test.lisp:12 "get_error_handler() returns the active handler and null when none is installed."
(it-sequential
    "get_error_handler() returns the active handler and null when none is installed."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ return true; } echo get_error_handler() === null ? 'null' : 'bad'; set_error_handler('h'); echo ':' . get_error_handler() . ':'; restore_error_handler(); echo get_error_handler() === null ? 'null' : 'bad';")
      :to-equal
      "null:h:null"))

;; t/runtime-builtins-io-objects-php85-test.lisp:19 "get_exception_handler() returns the active handler and null when none is installed."
(it-sequential
    "get_exception_handler() returns the active handler and null when none is installed."
    (expect
      (%php-run-capture
        "<?php function eh($e){} echo get_exception_handler() === null ? 'null' : 'bad'; set_exception_handler('eh'); echo ':' . get_exception_handler() . ':'; restore_exception_handler(); echo get_exception_handler() === null ? 'null' : 'bad';")
      :to-equal
      "null:eh:null"))

;; t/runtime-builtins-io-objects-php85-test.lisp:34 "The PHP 8.5 gc_collect_cycles() builtin executes from PHP source."
(it-sequential
    "The PHP 8.5 gc_collect_cycles() builtin executes from PHP source."
    (expect
      (%php-run-capture
        "<?php echo gettype(gc_collect_cycles()) . ':' . gc_collect_cycles();")
      :to-equal
      "integer:0"))

;; t/runtime-builtins-io-objects-php85-test.lisp:41 "The PHP 8.5 opcache file-cache probe executes from PHP source."
(it-sequential
    "The PHP 8.5 opcache file-cache probe executes from PHP source."
    (expect
      (%php-run-capture
        "<?php echo opcache_is_script_cached_in_file_cache('/tmp/example.php') ? 'warm' : 'cold';")
      :to-equal
      "cold"))

;; t/runtime-builtins-io-objects-php85-test.lisp:58 "The PHP 8.5 persistent cURL share initializer executes from PHP source."
(it-sequential
    "The PHP 8.5 persistent cURL share initializer executes from PHP source."
    (expect
      (%php-run-capture
        "<?php $a=curl_share_init_persistent('cache'); $b=curl_share_init_persistent('cache'); echo ($a === $b ? 'same' : 'diff') . ':' . gettype($a);")
      :to-equal
      "same:object"))

;; t/runtime-builtins-io-objects-php85-test.lisp:65 "The persistent cURL share handle is visible to class_exists before it is created."
(it-sequential
    "The persistent cURL share handle is visible to class_exists before it is created."
    (expect
      (%php-run-capture
        "<?php echo class_exists('CurlSharePersistentHandle') ? 'Y' : 'N';")
      :to-equal
      "Y"))

;; t/runtime-builtins-io-objects-php85-test.lisp:72 "PHP 8.5 object/boolean loose comparison follows the object's bool cast."
(it-sequential
    "PHP 8.5 object/boolean loose comparison follows the object's bool cast."
    (expect
      (%php-run-capture
        "<?php $o = curl_share_init_persistent('cmp'); $t = true; $f = false; echo ($o == true ? 'T' : 'F'); echo ($o == $t ? 'T' : 'F'); echo ($o == $f ? 'T' : 'F');")
      :to-equal
      "TTF")
    (let ((object (make-hash-table :test #'equal))
          (empty-array (cl-cc/php::%php-array)))
      (setf (gethash "__class__" object) "UncomparableInternal")
      (expect (cl-cc/php::%php-truthy object) :to-be-truthy)
      (expect (cl-cc/php::%php-eq-loose object t) :to-be-truthy)
      (expect (cl-cc/php::%php-eq-loose object nil) :to-be-falsy)
      (expect (cl-cc/php::%php-truthy empty-array) :to-be-falsy)))

;; t/runtime-builtins-io-objects-php85-test.lisp:98 "curl_multi_get_handles() is registered as a PHP 8.5 builtin."
(it-sequential
    "curl_multi_get_handles() is registered as a PHP 8.5 builtin."
    (expect
      (%php-run-capture
        "<?php echo function_exists('curl_multi_get_handles') ? 'Y' : 'N';")
      :to-equal
      "Y"))

;; t/runtime-builtins-io-objects-php85-test.lisp:105 "The PHP 8.5 Locale likely-subtag static methods lower to runtime helpers."
(it-sequential
    "The PHP 8.5 Locale likely-subtag static methods lower to runtime helpers."
    (expect
      (%php-run-capture
        "<?php echo Locale::addLikelySubtags('en') . ':' . Locale::minimizeSubtags('en_Latn_US');")
      :to-equal
      "en-Latn-US:en"))

;; t/runtime-builtins-io-objects-php85-test.lisp:151 "PHP 8.5 class_alias is registered for compiled PHP code."
(it-sequential
    "PHP 8.5 class_alias is registered for compiled PHP code."
    (let ((alias (format nil "Php85AliasSourceUri~A" (symbol-name (gensym)))))
      (expect
        (%php-run-capture
          (format
            nil
            "<?php echo class_alias('Uri\\Rfc3986\\Uri', '~A') && class_exists('~A') && method_exists('~A', 'parse') ? 'Y' : 'N';"
            alias
            alias
            alias))
        :to-equal
        "Y")))

;; t/runtime-builtins-io-objects-php85-test.lisp:177 "PHP 8.5 IntlListFormatter, NumberFormatter, and Pdo\\Sqlite class constants resolve."
(it-sequential
    "PHP 8.5 IntlListFormatter, NumberFormatter, and Pdo\\Sqlite class constants resolve."
    (expect
      (%php-run-capture
        "<?php echo IntlListFormatter::TYPE_AND . ':' . IntlListFormatter::TYPE_OR . ':' . NumberFormatter::CURRENCY_ISO . ':' . NumberFormatter::CURRENCY_PLURAL . ':' . NumberFormatter::CURRENCY_ACCOUNTING . ':' . NumberFormatter::CASH_CURRENCY . ':' . NumberFormatter::DECIMAL_COMPACT_SHORT . ':' . NumberFormatter::DECIMAL_COMPACT_LONG . ':' . NumberFormatter::CURRENCY_STANDARD . ':' . Pdo\\Sqlite::ATTR_OPEN_FLAGS . ':' . Pdo\\Sqlite::OPEN_READONLY . ':' . Pdo\\Sqlite::OPEN_READWRITE . ':' . Pdo\\Sqlite::OPEN_CREATE . ':' . Pdo\\Sqlite::ATTR_READONLY_STATEMENT . ':' . Pdo\\Sqlite::ATTR_EXTENDED_RESULT_CODES . ':' . Pdo\\Sqlite::ATTR_BUSY_STATEMENT . ':' . Pdo\\Sqlite::ATTR_EXPLAIN_STATEMENT . ':' . Pdo\\Sqlite::EXPLAIN_MODE_PREPARED . ':' . Pdo\\Sqlite::EXPLAIN_MODE_EXPLAIN . ':' . Pdo\\Sqlite::EXPLAIN_MODE_EXPLAIN_QUERY_PLAN . ':' . Pdo\\Sqlite::ATTR_TRANSACTION_MODE . ':' . Pdo\\Sqlite::TRANSACTION_MODE_DEFERRED . ':' . Pdo\\Sqlite::TRANSACTION_MODE_IMMEDIATE . ':' . Pdo\\Sqlite::TRANSACTION_MODE_EXCLUSIVE . ':' . Pdo\\Sqlite::DETERMINISTIC;")
      :to-equal
      "0:1:10:11:12:13:14:15:16:1000:1:2:4:1001:1002:1003:1004:0:1:2:1005:0:1:2:2048"))

;; t/runtime-builtins-io-objects-php85-test.lisp:217 "PHP 8.5 SoapClient, SoapFault, SoapServer, and XSLTProcessor methods execute from PHP source."
(it-sequential
    "PHP 8.5 SoapClient, SoapFault, SoapServer, and XSLTProcessor methods execute from PHP source."
    (expect
      (%php-run-capture
        "<?php
$client = new SoapClient(null, ['location' => 'http://example.test', 'uri' => 'urn:test']);
$fault = new SoapFault('Sender', 'Boom', 'actor', 'detail', 'name', 'header', 'ja');
$server = new SoapServer(null, ['uri' => 'urn:test']);
$proc = new XSLTProcessor();
$proc->setParameter('urn:a', 'foo', 'bar');
echo (is_array($client->__getTypes()) ? 'Y' : 'N') . ':' .
     ($fault->lang === 'ja' ? 'Y' : 'N') . ':' .
     $fault->lang . ':' .
     (is_null($server->fault('Client', 'Boom')) ? 'Y' : 'N') . ':' .
     ($proc->getParameter('urn:a', 'foo') === 'bar' ? 'Y' : 'N') . ':' .
     ($proc->getParameter('urn:b', 'foo') === false ? 'Y' : 'N') . ':' .
     ($proc->removeParameter('urn:a', 'foo') ? 'Y' : 'N');
")
      :to-equal
      "Y:Y:ja:Y:Y:Y:Y"))

;; t/runtime-builtins-io-objects-php85-test.lisp:237 "PHP 8.5 ReflectionConstant method additions execute from PHP source."
(it-sequential
    "PHP 8.5 ReflectionConstant method additions execute from PHP source."
    (expect
      (%php-run-capture
        "<?php
$r = new ReflectionConstant('PHP_VERSION');
echo ($r->isDeprecated() ? 'Y' : 'N') . ':' .
     (is_array($r->getAttributes()) ? 'Y' : 'N') . ':' .
     (is_null($r->getFileName()) ? 'Y' : 'N') . ':' .
     (is_null($r->getExtensionName()) ? 'Y' : 'N');
")
      :to-equal
      "N:Y:Y:Y"))

;; t/runtime-builtins-io-objects-php85-test.lisp:250 "PHP 8.5 ReflectionProperty::getMangledName executes from PHP source."
(it-sequential
    "PHP 8.5 ReflectionProperty::getMangledName executes from PHP source."
    (expect
      (%php-run-capture
        "<?php
class RProp {
    public $name;
}
$r = new ReflectionProperty('RProp', 'name');
echo $r->getMangledName();
")
      :to-equal
      "name"))

;; t/runtime-builtins-io-objects-php85-test.lisp:263 "%php-reflection-property-new always sets :public, so getMangledName never
actually mangles a private/protected property's name"
(it-sequential
    "%php-reflection-property-new always sets :public, so getMangledName never
actually mangles a private/protected property's name"
    ;; ReflectionProperty's constructor never looks up the target property's real
    ;; declared visibility — this runtime has no class-property-visibility registry
    ;; for it to consult — so it hardcodes :PUBLIC unconditionally, and the
    ;; :PRIVATE/:PROTECTED branches in %PHP-REFLECTION-PROPERTY-GET-MANGLED-NAME
    ;; are unreachable from any real PHP program. Real PHP mangles a private
    ;; property's name to "\0ClassName\0propname"; this runtime always returns the
    ;; bare name instead, even for a genuinely `private`-declared property.
    (expect
      (%php-run-capture
        "<?php
class RPriv {
    private $secret;
}
$r = new ReflectionProperty('RPriv', 'secret');
echo $r->getMangledName();
")
      :to-equal
      "secret"))

;; t/runtime-builtins-io-objects-php85-test.lisp:284 "PHP 8.5 Pdo\\Sqlite::setAuthorizer executes from PHP source."
(it-sequential
    "PHP 8.5 Pdo\\Sqlite::setAuthorizer executes from PHP source."
    (expect
      (%php-run-capture
        "<?php
$pdo = new Pdo\\Sqlite('sqlite::memory:');
echo is_null($pdo->setAuthorizer(null)) ? 'Y' : 'N';
")
      :to-equal
      "Y"))

;; t/runtime-builtins-io-objects-php85-test.lisp:294 "PHP 8.5 SQLite3Stmt::busy executes from PHP source."
(it-sequential
    "PHP 8.5 SQLite3Stmt::busy executes from PHP source."
    (expect
      (%php-run-capture
        "<?php
$stmt = new SQLite3Stmt();
echo $stmt->busy() ? 'Y' : 'N';
")
      :to-equal
      "N"))

;; t/runtime-builtins-io-tokenizer-php85-test.lisp:13 "PHP 8.5 IMAGETYPE_* constants match current extension values."
(it-sequential
    "PHP 8.5 IMAGETYPE_* constants match current extension values."
    (expect
      (%php-run-capture
        "<?php echo IMAGETYPE_UNKNOWN . ':' . IMAGETYPE_WEBP . ':' . IMAGETYPE_AVIF . ':' . IMAGETYPE_HEIF . ':' . IMAGETYPE_SVG . ':' . IMAGETYPE_COUNT;")
      :to-equal
      "0:18:19:20:21:22"))

;; t/runtime-builtins-io-tokenizer-php85-test.lisp:20 "PHP 8.5 image type helpers support current IMAGETYPE_* values."
(it-sequential
    "PHP 8.5 image type helpers support current IMAGETYPE_* values."
    (expect
      (%php-run-capture
        "<?php
echo image_type_to_extension(IMAGETYPE_SVG) . ':' .
     image_type_to_extension(IMAGETYPE_SVG, false) . ':' .
     image_type_to_mime_type(IMAGETYPE_SVG) . ':' .
     image_type_to_extension(IMAGETYPE_WEBP) . ':' .
     image_type_to_extension(IMAGETYPE_WEBP, false) . ':' .
     image_type_to_mime_type(IMAGETYPE_WEBP) . ':' .
     image_type_to_extension(IMAGETYPE_AVIF) . ':' .
     image_type_to_extension(IMAGETYPE_AVIF, false) . ':' .
     image_type_to_mime_type(IMAGETYPE_AVIF) . ':' .
     image_type_to_extension(IMAGETYPE_HEIF) . ':' .
     image_type_to_extension(IMAGETYPE_HEIF, false) . ':' .
     image_type_to_mime_type(IMAGETYPE_HEIF) . ':' .
     (function_exists('image_type_to_extension') ? 'Y' : 'N') . ':' .
     (function_exists('image_type_to_mime_type') ? 'Y' : 'N') . ':' .
     (image_type_to_extension(9999) === false ? 'false' : 'other');
")
      :to-equal
      ".svg:svg:image/svg+xml:.webp:webp:image/webp:.avif:avif:image/avif:.heif:heif:image/heif:Y:Y:false"))

;; t/runtime-builtins-io-tokenizer-php85-test.lisp:43 "PHP 8.5 getimagesize returns SVG dimensions, units, MIME, and image type."
(it-sequential
    "PHP 8.5 getimagesize returns SVG dimensions, units, MIME, and image type."
    (expect
      (%php-run-capture
        "<?php
$f = tempnam(sys_get_temp_dir(), 'clcc-svg-');
file_put_contents($f, '<svg width=\"12cm\" height=\"34px\" xmlns=\"http://www.w3.org/2000/svg\"></svg>');
$size = getimagesize($f);
echo $size[0] . ':' .
     $size[1] . ':' .
     ($size[2] === IMAGETYPE_SVG ? 'type' : 'bad') . ':' .
     $size['mime'] . ':' .
     $size['width_unit'] . ':' .
     $size['height_unit'] . ':' .
     (exif_imagetype($f) === IMAGETYPE_SVG ? 'exif' : 'bad') . ':' .
     (getimagesize($f . '.missing') === false ? 'false' : 'other') . ':' .
     (function_exists('getimagesize') ? 'Y' : 'N') . ':' .
     (function_exists('exif_imagetype') ? 'Y' : 'N');
unlink($f);
")
      :to-equal
      "12:34:type:image/svg+xml:cm:px:exif:false:Y:Y"))

;; t/runtime-builtins-io-tokenizer-php85-test.lisp:345 "PHP 8.5 tokenizer builtins are callable from PHP code."
(it-sequential
    "PHP 8.5 tokenizer builtins are callable from PHP code."
    (expect
      (%php-run-capture
        "<?php echo token_name(T_PIPE) . ':' . token_name(T_VOID_CAST) . ':' . token_name(T_INLINE_HTML) . ':' . token_name(T_CLOSE_TAG) . ':' . token_name(T_YIELD_FROM) . ':' . token_name(T_ATTRIBUTE) . ':' . token_name(T_NAME_FULLY_QUALIFIED) . ':' . token_name(T_NAME_QUALIFIED) . ':' . token_name(T_NAME_RELATIVE) . ':' . token_name(T_NS_SEPARATOR) . ':' . token_name(T_BAD_CHARACTER);")
      :to-equal
      "T_PIPE:T_VOID_CAST:T_INLINE_HTML:T_CLOSE_TAG:T_YIELD_FROM:T_ATTRIBUTE:T_NAME_FULLY_QUALIFIED:T_NAME_QUALIFIED:T_NAME_RELATIVE:T_NS_SEPARATOR:T_BAD_CHARACTER"))

;; t/runtime-builtins-io-tokenizer-php85-test.lisp:392 "filter_var() supports integer validation."
(it-sequential
    "filter_var() supports integer validation."
    (expect
      (%php-run-capture
        "<?php echo filter_var('42', FILTER_VALIDATE_INT) . ':' . (filter_var('abc', FILTER_VALIDATE_INT) === false ? 'false' : 'bad');")
      :to-equal
      "42:false"))

;; t/runtime-builtins-io-tokenizer-php85-test.lisp:399 "FILTER_VALIDATE_BOOLEAN can return a successful false result."
(it-sequential
    "FILTER_VALIDATE_BOOLEAN can return a successful false result."
    (expect
      (%php-run-capture
        "<?php echo filter_var('false', FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE) === false ? 'false' : 'bad';")
      :to-equal
      "false"))

;; t/runtime-builtins-io-uri-php85-test.lisp:81 "PHP 8.5 URI equality excludes fragments by default and includes them with UriComparisonMode::IncludeFragment."
(it-sequential
    "PHP 8.5 URI equality excludes fragments by default and includes them with UriComparisonMode::IncludeFragment."
    (let ((a (cl-cc/php::%php-uri-rfc3986-new "https://example.com/a#one"))
          (b (cl-cc/php::%php-uri-rfc3986-new "https://example.com/a#two")))
      (expect (cl-cc/php::%php-uri-equals a b) :to-be-truthy)
      (expect
        (cl-cc/php::%php-uri-equals
          a
          b
          (cl-cc/php:%php-predefined-class-constant
            "Uri\\UriComparisonMode"
            "IncludeFragment"))
        :to-be-falsy))
    (expect
      (%php-run-capture
        "<?php
$a = new Uri\\Rfc3986\\Uri('https://example.com/a#one');
$b = new Uri\\Rfc3986\\Uri('https://example.com/a#two');
echo ($a->equals($b) ? 'Y' : 'N') . ':' . ($a->equals($b, Uri\\UriComparisonMode::IncludeFragment) ? 'Y' : 'N');
")
      :to-equal
      "Y:N"))

;; t/runtime-builtins-io-uri-php85-test.lisp:110 "PHP 8.5 object debug output uses __debugInfo for Uri objects."
(it-sequential
    "PHP 8.5 object debug output uses __debugInfo for Uri objects."
    (expect
      (%php-run-capture
        "<?php
$u = new Uri\\Rfc3986\\Uri('https://user@example.com/a?b=1#frag');
var_dump($u);
")
      :to-equal
      "object(Uri\\Rfc3986\\Uri) (8) {
  [\"scheme\"]=>
  string(5) \"https\"
  [\"username\"]=>
  string(4) \"user\"
  [\"password\"]=>
  NULL
  [\"host\"]=>
  string(11) \"example.com\"
  [\"port\"]=>
  NULL
  [\"path\"]=>
  string(2) \"/a\"
  [\"query\"]=>
  string(3) \"b=1\"
  [\"fragment\"]=>
  string(4) \"frag\"
}")
    (expect
      (%php-run-capture
        "<?php
$u = new Uri\\Rfc3986\\Uri('https://user@example.com/a?b=1#frag');
echo print_r($u, true);
")
      :to-equal
      "Uri\\Rfc3986\\Uri Object (
    [scheme] => https
    [username] => user
    [password] => NULL
    [host] => example.com
    [port] => NULL
    [path] => /a
    [query] => b=1
    [fragment] => frag
)"))

;; t/runtime-builtins-io-uri-php85-test.lisp:154 "PHP 8.5 URI constructor and static parse helpers execute from PHP source."
(it-sequential
    "PHP 8.5 URI constructor and static parse helpers execute from PHP source."
    (expect
      (%php-run-capture
        "<?php
$u = new Uri\\Rfc3986\\Uri('https://user@example.com/a?b=1#frag');
$v = $u->withUserInfo('ada');
$w = Uri\\WhatWg\\Url::parse('https://example.com/p');
echo $u->getScheme() . ':' . $u->getHost() . ':' . $u->getPath() . ':' . $u->getQuery() . ':' . $u->getFragment() . ':' . $u->getUsername() . ':' . $v->getUsername() . ':' . $w->toAsciiString();
")
      :to-equal
      "https:example.com:/a:b=1:frag:user:ada:https://example.com/p"))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:27 "php-e2e-math-non-cl-named-builtins: ~S"
(it-sequential-each
      (("<?php echo fmod(7,3);"
        "1")
       ("<?php echo fmod(-7,3);"
        "-1")
       ("<?php echo round(atan2(1,1),4);"
        "0.7854")
       ("<?php echo log10(1000);"
        "3")
       ("<?php echo log2(8);"
        "3")
       ("<?php echo hypot(3,4);"
        "5")
       ("<?php echo round(deg2rad(180),5);"
        "3.14159")
       ("<?php echo round(rad2deg(3.141592653589793),2);"
        "180")
       ("<?php echo base_convert('ff',16,2);"
        "11111111")
       ("<?php echo base_convert('255',10,16);"
        "ff")
       ("<?php echo is_finite(1.5)?'y':'n';"
        "y")
       ("<?php echo is_infinite(fdiv(1,0))?'y':'n';"
        "y"))
      "php-e2e-math-non-cl-named-builtins: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:55 "php-e2e-serialize-unserialize: ~S"
(it-sequential-each
      (("<?php echo serialize(42);"
        "i:42;")
       ("<?php echo serialize('hello');"
        "s:5:\"hello\";")
       ("<?php echo serialize(true);"
        "b:1;")
       ("<?php echo serialize(false);"
        "b:0;")
       ("<?php echo serialize(null);"
        "N;")
       ("<?php echo serialize(3.14);"
        "d:3.14;")
       ("<?php echo serialize([1,2,3]);"
        "a:3:{i:0;i:1;i:1;i:2;i:2;i:3;}")
       ("<?php echo serialize(['a'=>1,'b'=>2]);"
        "a:2:{s:1:\"a\";i:1;s:1:\"b\";i:2;}")
       ("<?php class C { public $x = 7; } echo serialize(new C());"
        "O:1:\"C\":1:{s:1:\"x\";i:7;}")
       ("<?php class C { public $x = 7; public $y = 9; function __sleep(){ return ['y','x']; } } echo serialize(new C());"
        "O:1:\"C\":2:{s:1:\"y\";i:9;s:1:\"x\";i:7;}")
       ("<?php class C { public $x = 7; public $y = 9; function __sleep(){ return ['y']; } function __wakeup(){ $this->x = 6; } } $u = unserialize(serialize(new C())); echo $u->x + $u->y;"
        "15")
       ("<?php class C { public $x = 7; public $y = 9; function __serialize(){ return ['y'=>$this->y, 'x'=>$this->x]; } } echo serialize(new C());"
        "O:1:\"C\":2:{s:1:\"y\";i:9;s:1:\"x\";i:7;}")
       ("<?php class C { public $x = 7; public $y = 9; function __serialize(){ return ['y'=>$this->y, 'x'=>$this->x]; } function __unserialize($data){ $this->y = $data['y']; $this->x = $data['x'] - 1; } } $u = unserialize(serialize(new C())); echo $u->x + $u->y;"
        "15")
       ("<?php echo unserialize(serialize(42))+8;"
        "50")
       ("<?php $x=unserialize(serialize([1,2,['k'=>'v']])); echo $x[2]['k'];"
        "v")
       ("<?php class C { public $x = 7; } $u = unserialize(serialize(new C())); echo $u->x;"
        "7")
       ("<?php echo unserialize('b:1;')?'T':'F';"
        "T")
       ("<?php echo unserialize('garbage')?'T':'F';"
        "F"))
      "php-e2e-serialize-unserialize: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:95 "php-e2e-string-escape-preservation: ~S"
(it-sequential-each
      (("<?php echo \"a\\.b\";"
        "a\\.b")
       ("<?php echo \"\\d\\w\";"
        "\\d\\w")
       ("<?php echo strlen(\"a\\nb\");"
        "3")
       ("<?php echo \"\\x41\";"
        "A")
       ("<?php echo \"\\u{48}\";"
        "H")
       ("<?php echo strlen(\"\\t\");"
        "1"))
      "php-e2e-string-escape-preservation: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:111 "php-e2e-md5-sha1-builtins: ~S"
(it-sequential-each
      (("<?php echo md5('abc');"
        "900150983cd24fb0d6963f7d28e17f72")
       ("<?php echo md5('');"
        "d41d8cd98f00b204e9800998ecf8427e")
       ("<?php echo bin2hex(md5('abc', true));"
        "900150983cd24fb0d6963f7d28e17f72")
       ("<?php echo sha1('abc');"
        "a9993e364706816aba3e25717850c26c9cd0d89d")
       ("<?php echo sha1('');"
        "da39a3ee5e6b4b0d3255bfef95601890afd80709")
       ("<?php echo bin2hex(sha1('abc', true));"
        "a9993e364706816aba3e25717850c26c9cd0d89d"))
      "php-e2e-md5-sha1-builtins: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:127 "php-e2e-crc32-builtin: ~S"
(it-sequential-each
      (("<?php echo crc32('');"
        "0")
       ("<?php echo crc32('abc');"
        "891568578")
       ("<?php echo crc32('foo');"
        "2356372769"))
      "php-e2e-crc32-builtin: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:137 "php-e2e-preg-replace-callback: ~S"
(it-sequential-each
      (("<?php echo preg_replace_callback('/\\d/', fn($m)=>$m[0]*2, 'a1b2');"
        "a2b4")
       ("<?php echo preg_replace_callback('/[a-z]+/', fn($m)=>strtoupper($m[0]), 'hi there');"
        "HI THERE")
       ("<?php echo preg_replace_callback('/\\d/', fn($m)=>'X', '1234', 2);"
        "XX34")
       ("<?php echo preg_replace_callback(\"/\\d/\", fn($m)=>$m[0]*2, 'a1b2');"
        "a2b4")
       ("<?php echo preg_replace_callback('/(\\w+)=(\\d+)/', fn($m)=>$m[1].':'.$m[2], 'x=12 y=34');"
        "x:12 y:34")
       ("<?php echo preg_replace_callback('/((\\d)(\\d))/', fn($m)=>$m[2].'-'.$m[3], '42');"
        "4-2")
       ("<?php echo preg_replace_callback_array(['/\\d/'=>fn($m)=>'N','/[a-z]/'=>fn($m)=>'L'], 'a1b2');"
        "LNLN")
       ("<?php echo preg_replace_callback_array(['/(\\w)(\\d)/'=>fn($m)=>$m[2].$m[1]], 'a1 b2');"
        "1a 2b"))
      "php-e2e-preg-replace-callback: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:157 "php-e2e-preg-capture-groups: ~S"
(it-sequential-each
      (("<?php echo preg_replace('/(\\w)(\\w)/', '$2$1', 'abcd');"
        "badc")
       ("<?php echo preg_replace('/(\\d+)-(\\d+)/', '$2/$1', '12-34');"
        "34/12")
       ("<?php echo preg_replace('/\\d+/', '[$0]', 'a12b');"
        "a[12]b")
       ("<?php echo preg_replace('/(\\w+)/', '${1}!', 'hi');"
        "hi!")
       ("<?php echo preg_replace('/((\\d)(\\d))/', '$2-$3', '42');"
        "4-2")
       ("<?php echo preg_match_all('/\\d/', '1a2b3');"
        "3")
       ("<?php echo preg_match_all('/\\w+/', 'foo bar baz');"
        "3")
       ("<?php echo preg_match_all('/\\d/', 'abc');"
        "0"))
      "php-e2e-preg-capture-groups: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:177 "php-e2e-preg-match-out-param: ~S"
(it-sequential-each
      (("<?php preg_match('/(\\d+)-(\\d+)/', '12-34', $m); echo $m[1].'|'.$m[2];"
        "12|34")
       ("<?php $r=preg_match('/(\\d+)/', 'abc123', $m); echo $r.':'.$m[0].':'.$m[1];"
        "1:123:123")
       ("<?php preg_match('/(\\w+)@(\\w+)/', 'bob@host', $m); echo $m[1].' at '.$m[2];"
        "bob at host")
       ("<?php echo preg_match('/\\d/', 'abc', $m);"
        "0")
       ("<?php preg_match_all('/(\\d)/', '1a2b3', $m); echo implode(',', $m[1]);"
        "1,2,3")
       ("<?php preg_match_all('/\\d+/', 'a12b34', $m); echo implode(',', $m[0]);"
        "12,34")
       ("<?php $r=preg_match('/\\d/', 'a1b2c3', $m, 0, 3); echo $r.':'.$m[0];"
        "1:2")
       ("<?php preg_match_all('/(\\d)/', 'a1b2c3', $m, 0, 3); echo implode(',', $m[1]);"
        "2,3"))
      "php-e2e-preg-match-out-param: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:197 "php-e2e-ucwords-delimiters: ~S"
(it-sequential-each
      (("<?php echo ucwords('hello world');"
        "Hello World")
       ("<?php echo ucwords('world order roar');"
        "World Order Roar")
       ("<?php echo ucwords('fluffy vivid');"
        "Fluffy Vivid")
       ("<?php echo ucwords('the QUICK brown');"
        "The QUICK Brown")
       ("<?php echo ucwords('hello-world', '-');"
        "Hello-World"))
      "php-e2e-ucwords-delimiters: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:211 "php-e2e-wordwrap-cut: ~S"
(it-sequential-each
      (("<?php echo wordwrap('aaaaaa',3,'|',true);"
        "aaa|aaa")
       ("<?php echo wordwrap('a verylongword b',4,'|',true);"
        "a|very|long|word|b")
       ("<?php echo wordwrap('A very long woooooooord.',8,'|',true);"
        "A very|long|wooooooo|ord.")
       ("<?php echo wordwrap('aaaaaa',3,'|',false);"
        "aaaaaa")
       ("<?php echo wordwrap('aaa bbb ccc',5,'|');"
        "aaa|bbb|ccc")
       ("<?php echo wordwrap('The quick brown fox',10,'|');"
        "The quick|brown fox"))
      "php-e2e-wordwrap-cut: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:232 "php-e2e-json-decode: ~S"
(it-sequential-each
      (("<?php $d=json_decode('{\"a\":1,\"b\":2}',true); echo $d['a']+$d['b'];"
        "3")
       ("<?php $d=json_decode('{\"name\":\"Bob\",\"age\":30}',true); echo $d['name'].'-'.$d['age'];"
        "Bob-30")
       ("<?php $d=json_decode('{\"x\":{\"y\":[1,2,3]}}',true); echo $d['x']['y'][2];"
        "3")
       ("<?php $d=json_decode('[\"a\",\"bb\",\"ccc\"]'); echo $d[1].strlen($d[2]);"
        "bb3")
       ("<?php echo json_decode('true')?'y':'n';"
        "y")
       ("<?php echo json_decode('not json')===null?'null':'x';"
        "null")
       ("<?php $o=['user'=>'alice','roles'=>['admin','editor'],'active'=>true]; $r=json_decode(json_encode($o),true); echo $r['user'].':'.$r['roles'][1].':'.($r['active']?'y':'n');"
        "alice:editor:y"))
      "php-e2e-json-decode: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:250 "php-e2e-number-format-rounding: ~S"
(it-sequential-each
      (("<?php echo number_format(1234.5);"
        "1,235")
       ("<?php echo number_format(2.5);"
        "3")
       ("<?php echo number_format(0.5);"
        "1")
       ("<?php echo number_format(1234.4);"
        "1,234")
       ("<?php echo number_format(-1234.5);"
        "-1,235")
       ("<?php echo number_format(3.14159,2);"
        "3.14")
       ("<?php echo number_format(1234.567,2);"
        "1,234.57")
       ("<?php echo number_format(1234.5,2,',','.');"
        "1.234,50"))
      "php-e2e-number-format-rounding: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:270 "php-e2e-round-half-away: ~S"
(it-sequential-each
      (("<?php echo round(2.5);"
        "3")
       ("<?php echo round(3.5);"
        "4")
       ("<?php echo round(-2.5);"
        "-3")
       ("<?php echo round(0.5);"
        "1")
       ("<?php echo round(2.4);"
        "2")
       ("<?php echo round(-2.4);"
        "-2")
       ("<?php echo round(3.14159,2);"
        "3.14")
       ("<?php echo round(1.95583,2);"
        "1.96")
       ("<?php echo round(1241757,-3);"
        "1242000"))
      "php-e2e-round-half-away: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:292 "php-e2e-sprintf-flags: ~S"
(it-sequential-each
      (("<?php echo sprintf('%e',12345.678);"
        "1.234568e+4")
       ("<?php echo sprintf('%E',0.00012);"
        "1.200000E-4")
       ("<?php echo sprintf('%+d %+d',5,-5);"
        "+5 -5")
       ("<?php echo sprintf('%+05d',42);"
        "+0042")
       ("<?php echo sprintf('%05d',-42);"
        "-0042")
       ("<?php echo sprintf(\"%'*10d\",42);"
        "********42")
       ("<?php echo sprintf(\"%'-10s\",'hi');"
        "--------hi")
       ("<?php echo sprintf('%05d',42);"
        "00042")
       ("<?php echo sprintf('[%-5s]','ab');"
        "[ab   ]")
       ("<?php echo sprintf('%.2f',3.14159);"
        "3.14")
       ("<?php echo sprintf('%x %X %o',255,255,8);"
        "ff FF 10"))
      "php-e2e-sprintf-flags: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:318 "php-e2e-base-conversions: ~S"
(it-sequential-each
      (("<?php echo dechex(255);"
        "ff")
       ("<?php echo hexdec('ff');"
        "255")
       ("<?php echo decbin(10);"
        "1010")
       ("<?php echo bindec('1010');"
        "10")
       ("<?php echo decoct(64);"
        "100")
       ("<?php echo octdec('100');"
        "64")
       ("<?php echo dechex(-1);"
        "ffffffffffffffff")
       ("<?php echo hexdec(dechex(48879));"
        "48879"))
      "php-e2e-base-conversions: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:338 "php-e2e-max-min: ~S"
(it-sequential-each
      (("<?php echo max([3,1,2]);"
        "3")
       ("<?php echo min([3,1,2]);"
        "1")
       ("<?php echo max(1,5,3);"
        "5")
       ("<?php echo max(1,'10',5);"
        "10")
       ("<?php echo min(2.5,2,3);"
        "2")
       ("<?php echo max(42);"
        "42"))
      "php-e2e-max-min: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-formatting-test.lisp:15 "php-e2e-var-dump-and-export"
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

;; t/runtime-builtins-register-e2e-formatting-test.lisp:227 "php-e2e-json-encode-pretty"
(it-sequential "php-e2e-json-encode-pretty"
  (expect (%php-run-capture "<?php echo json_encode(['a'=>1,'b'=>[2,3]]);") :to-equal "{\"a\":1,\"b\":[2,3]}")
  (expect (%php-run-capture "<?php echo json_encode(['a'=>1,'b'=>[2,3]],JSON_PRETTY_PRINT);") :to-equal (format nil "{~%    \"a\": 1,~%    \"b\": [~%        2,~%        3~%    ]~%}"))
  (expect (%php-run-capture "<?php echo json_encode([1,2,3],JSON_PRETTY_PRINT);") :to-equal (format nil "[~%    1,~%    2,~%    3~%]"))
  (expect (%php-run-capture "<?php echo json_encode(['x'=>[],'y'=>1],JSON_PRETTY_PRINT);") :to-equal (format nil "{~%    \"x\": [],~%    \"y\": 1~%}")))

;; t/runtime-builtins-register-e2e-test.lisp:102 "php-e2e-extract-static-array-literal: ~S"
(it-sequential-each
      (("<?php echo function_exists('extract')?'present':'absent';"
        "present")
       ("<?php extract(['a'=>1,'b'=>'two','_c'=>3,'bad-key'=>4,5=>6]); echo $a.':'.$b.':'.$_c.':'.(isset($bad)?'y':'n');"
        "1:two:3:n")
       ("<?php $a='old'; extract(['a'=>'new']); echo $a;"
        "new"))
      "php-e2e-extract-static-array-literal: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-test.lisp:114 "php-e2e-empty-undefined-variable: ~S"
(it-sequential-each
      (("<?php echo empty($missing)?'empty':'set';"
        "empty")
       ("<?php $a=0; $b='value'; $c=null; echo (empty($a)?'empty':'set').':'.(empty($b)?'empty':'set').':'.(empty($c)?'empty':'set');"
        "empty:set:empty"))
      "php-e2e-empty-undefined-variable: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-test.lisp:140 "php-e2e-compact-captures-static-visible-variables: ~S"
(it-sequential-each
      (("<?php echo function_exists('compact')?'present':'absent';"
        "present")
       ("<?php $name='Ada'; $age=36; $r=compact('name','age','missing'); echo $r['name'].':'.$r['age'];"
        "Ada:36")
       ("<?php $x='yes'; $r=compact(['x']); echo $r['x'];"
        "yes")
       ("<?php $x='ok'; $y=7; $r=compact(['x',['y']]); echo $r['x'].':'.$r['y'];"
        "ok:7"))
      "php-e2e-compact-captures-static-visible-variables: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-test.lisp:154 "php-e2e-settype-mutates-variable: ~S"
(it-sequential-each
      (("<?php $x=5; settype($x,'string'); echo gettype($x).':'.$x;"
        "string:5")
       ("<?php $x='12abc'; settype($x,'integer'); echo gettype($x).':'.$x;"
        "integer:12")
       ("<?php $x='0'; settype($x,'bool'); echo gettype($x).':'.($x?'true':'false');"
        "boolean:false")
       ("<?php $x='value'; settype($x,'null'); echo gettype($x).':'.$x;"
        "NULL:")
       ("<?php $x=7; settype($x,'array'); echo gettype($x).':'.$x[0];"
        "array:7")
       ("<?php $x=7; settype($x,'object'); echo gettype($x).':'.get_class($x).':'.$x->scalar;"
        "object:stdClass:7")
       ("<?php $x=['name'=>'ada']; settype($x,'object'); echo gettype($x).':'.get_class($x).':'.$x->name;"
        "object:stdClass:ada")
       ("<?php $x=9; echo settype($x,'bogus')?'ok':'fail'; echo ':'.$x;"
        "fail:9"))
      "php-e2e-settype-mutates-variable: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-test.lisp:176 "php-e2e-sscanf-format-parsing: ~S"
(it-sequential-each
      (("<?php $v=sscanf('12 bob 3.5','%d %s %f'); echo $v[0].':'.$v[1].':'.$v[2];"
        "12:bob:3.5")
       ("<?php $r=sscanf('12-bob','%d-%s',$id,$name); echo $r.':'.$id.':'.$name;"
        "2:12:bob")
       ("<?php $v=sscanf('abcdef','%2c%2s'); echo $v[0].':'.$v[1];"
        "ab:cd")
       ("<?php $v=sscanf('ff 010','%x %i'); echo $v[0].':'.$v[1];"
        "255:8")
       ("<?php $r=sscanf('42 nope','%d:%s',$n,$s); echo $r.':'.$n.':'.$s;"
        "1:42:"))
      "php-e2e-sscanf-format-parsing: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-test.lisp:191 "php-e2e-sscanf-suppress-literal-octal-unsigned-and-float-aliases: ~S"
(it-sequential-each
      (("<?php $v=sscanf('1 2 3','%d %*d %d'); echo count($v).':'.$v[0].':'.$v[1];"
        "2:1:3")
       ("<?php $v=sscanf('50%','%d%%'); echo count($v).':'.$v[0];"
        "1:50")
       ("<?php $v=sscanf('17','%o'); echo $v[0];"
        "15")
       ("<?php $v=sscanf('-5','%u'); echo count($v);"
        "0")
       ("<?php $v=sscanf('3.5','%e'); echo $v[0];"
        "3.5")
       ("<?php $v=sscanf('3.5','%g'); echo $v[0];"
        "3.5"))
      "php-e2e-sscanf-suppress-literal-octal-unsigned-and-float-aliases: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-test.lisp:209 "php-e2e-intval-base: ~S"
(it-sequential-each
      (("<?php echo intval('42');"
        "42")
       ("<?php echo intval('1A',16);"
        "26")
       ("<?php echo intval('0x1A',16);"
        "26")
       ("<?php echo intval('077',8);"
        "63")
       ("<?php echo intval('1010',2);"
        "10")
       ("<?php echo intval('0x1A',0);"
        "26")
       ("<?php echo intval('017',0);"
        "15")
       ("<?php echo intval('0b101',0);"
        "5")
       ("<?php echo intval('-FF',16);"
        "-255")
       ("<?php echo intval('42abc');"
        "42")
       ("<?php echo intval('42',10);"
        "42"))
      "php-e2e-intval-base: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-test.lisp:237 "php-e2e-gmdate-weekday: ~S"
(it-sequential-each
      (("<?php echo gmdate('D',0);"
        "Thu")
       ("<?php echo gmdate('l',0);"
        "Thursday")
       ("<?php echo gmdate('N',0);"
        "4")
       ("<?php echo gmdate('w',0);"
        "4")
       ("<?php echo gmdate('D',86400*3);"
        "Sun")
       ("<?php echo gmdate('w',86400*3);"
        "0")
       ("<?php echo gmdate('N',86400*3);"
        "7")
       ("<?php echo gmdate('g:i A',3661);"
        "1:01 AM")
       ("<?php echo gmdate('Y-m-d H:i:s',86400);"
        "1970-01-02 00:00:00")
       ("<?php echo gmdate('l, F j, Y',0);"
        "Thursday, January 1, 1970"))
      "php-e2e-gmdate-weekday: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-test.lisp:263 "php-e2e-gmdate-formats: ~S"
(it-sequential-each
      (("<?php echo gmdate('L',0);"
        "0")
       ("<?php echo gmdate('L',63072000);"
        "1")
       ("<?php echo gmdate('t',0);"
        "31")
       ("<?php echo gmdate('t',2678400);"
        "28")
       ("<?php echo gmdate('t',65750400);"
        "29")
       ("<?php echo gmdate('z',0);"
        "0")
       ("<?php echo gmdate('z',86400);"
        "1")
       ("<?php echo gmdate('jS',0);"
        "1st")
       ("<?php echo gmdate('jS',86400);"
        "2nd")
       ("<?php echo gmdate('jS',172800);"
        "3rd")
       ("<?php echo gmdate('jS',864000);"
        "11th")
       ("<?php echo gmdate('jS',1728000);"
        "21st"))
      "php-e2e-gmdate-formats: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-test.lisp:293 "php-e2e-date-construct-and-udiff: ~S"
(it-sequential-each
      (("<?php echo gmmktime(0,0,0,1,2,1970);"
        "86400")
       ("<?php echo gmdate('Y-m-d',mktime(0,0,0,12,25,2000));"
        "2000-12-25")
       ("<?php echo json_encode(array_values(array_udiff([1,2,3,4],[2,4],fn($a,$b)=>$a-$b)));"
        "[1,3]")
       ("<?php echo json_encode(array_values(array_uintersect([1,2,3,4],[2,4,5],fn($a,$b)=>$a-$b)));"
        "[2,4]")
       ("<?php echo json_encode(array_values(array_udiff(['A','b','C'],['a','B'],fn($x,$y)=>strcasecmp($x,$y))));"
        "[\"C\"]"))
      "php-e2e-date-construct-and-udiff: ~S"
      (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-builtins-register-e2e-test.lisp:35 "php-e2e-symbol-registered-builtins"
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

;; t/runtime-builtins-register-e2e-test.lisp:54 "php-e2e-math-builtins-are-symbol-registered"
(it-sequential "php-e2e-math-builtins-are-symbol-registered"
  (dolist (name '("sin" "cos" "tan" "log" "exp" "asin" "acos" "atan"
                  "sinh" "cosh" "tanh" "is_nan" "mt_srand" "srand"))
    (expect (cl-cc/php::%php-lookup-builtin-symbol name) :to-be-truthy))
  (expect (%php-run-capture "<?php echo round(sin('1.5707963267948966'));") :to-equal "1")
  (expect (%php-run-capture "<?php echo round(log('8', '2'));") :to-equal "3"))

;; t/runtime-builtins-register-e2e-test.lisp:62 "php-e2e-deprecated-each-is-absent"
(it-sequential "php-e2e-deprecated-each-is-absent"
  (expect (%php-run-capture
                   "<?php echo function_exists('each')?'present':'absent';") :to-equal "absent"))

;; t/runtime-builtins-register-e2e-test.lisp:67 "php-e2e-nl-langinfo-locale-metadata"
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

;; t/runtime-builtins-register-e2e-test.lisp:79 "php-e2e-header-response-model"
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

;; t/runtime-builtins-register-e2e-test.lisp:124 "php-e2e-scanf-reads-standard-input"
(it-sequential "php-e2e-scanf-reads-standard-input"
  (expect (%php-run-capture
                   "<?php echo function_exists('scanf')?'present':'absent';") :to-equal "present")
  (let ((*standard-input* (make-string-input-stream "12 bob 3.5")))
    (expect (%php-run-capture
                     "<?php $v=scanf('%d %s %f'); echo $v[0].':'.$v[1].':'.$v[2];") :to-equal "12:bob:3.5"))
  (let ((*standard-input* (make-string-input-stream "42-ada")))
    (expect (%php-run-capture
                     "<?php $r=scanf('%d-%s',$id,$name); echo $r.':'.$id.':'.$name;") :to-equal "2:42:ada")))

;; t/runtime-builtins-register-e2e-test.lisp:135 "php-e2e-standard-stream-constants-are-backed-by-streams"
(it-sequential "php-e2e-standard-stream-constants-are-backed-by-streams"
  (expect (%php-run-capture
                   "<?php echo (STDIN===null?'bad':'in'); echo ':'; fwrite(STDOUT,'out'); echo ':'; echo (STDERR===null?'bad':'err');") :to-equal "in:out:err"))

;; t/runtime-builtins-string-analysis-test.lisp:94 "php-e2e-similar-text-return-value"
(it-sequential "php-e2e-similar-text-return-value"
  (expect (%php-run-capture "<?php echo similar_text('World', 'Word');")
          :to-equal "4"))

;; t/runtime-builtins-string-analysis-test.lisp:98 "php-e2e-similar-text-percent-out-param"
(it-sequential "php-e2e-similar-text-percent-out-param"
  (expect (%php-run-capture
           "<?php $p = null; similar_text('World', 'Word', $p); echo round($p, 2);")
          :to-equal "88.89"))

;; t/runtime-builtins-string-analysis-test.lisp:117 "php-e2e-printf-writes-formatted-output-and-returns-its-length"
(it-sequential "php-e2e-printf-writes-formatted-output-and-returns-its-length"
  (expect (%php-run-capture "<?php $n = printf('%d-%s', 5, 'x'); echo '|' . $n;")
          :to-equal "5-x|3"))

;; t/runtime-builtins-string-analysis-test.lisp:121 "php-e2e-vsprintf-formats-from-an-array-without-writing"
(it-sequential "php-e2e-vsprintf-formats-from-an-array-without-writing"
  (expect (%php-run-capture "<?php echo vsprintf('%d-%s', [5, 'x']);")
          :to-equal "5-x"))

;; t/runtime-builtins-string-analysis-test.lisp:125 "php-e2e-vprintf-writes-formatted-output-and-returns-its-length"
(it-sequential "php-e2e-vprintf-writes-formatted-output-and-returns-its-length"
  (expect (%php-run-capture "<?php $n = vprintf('%d-%s', [5, 'x']); echo '|' . $n;")
          :to-equal "5-x|3"))

;; t/runtime-helpers-e2e-test.lisp:23 "php-e2e-foreach-by-reference-mutates-array: ~S"
(it-sequential-each
    (("<?php $a=[1,2]; foreach ($a as &$v) { $v=$v*10; } echo $a[0].','.$a[1];"
        "10,20")
      ("<?php $a=[1=>2]; foreach ($a as $k=>&$v) { $v=$k+$v; } echo $a[1];" "3"))
    "php-e2e-foreach-by-reference-mutates-array: ~S"
    (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-helpers-e2e-test.lisp:37 "php-e2e-by-reference-function-metadata-is-source-local: ~S"
(it-sequential-each
    (("<?php function f(&$x) { $x=$x+1; } $n=1; f($n); echo $n;" "2")
      ("<?php function f($a,$b=10) { return $a+$b; } echo f(5);" "15")
      ("<?php function f($a,...$rest) { return $a.':'.count($rest); } echo f('x',1,2,3);"
        "x:3"))
    "php-e2e-by-reference-function-metadata-is-source-local: ~S"
    (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-helpers-e2e-test.lisp:45 "php-e2e-reference-assignment-aliases-variables: ~S"
(it-sequential-each
    (("<?php $a=1; $b=&$a; $a=2; echo $b;" "2")
      ("<?php $a=1; $b=&$a; $b=3; echo $a.','.$b;" "3,3"))
    "php-e2e-reference-assignment-aliases-variables: ~S"
    (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-helpers-e2e-test.lisp:105 "php-e2e-by-reference-parameters-literal-callables: ~S"
(it-sequential-each
    (("<?php $n=1; (function (&$x) { $x = $x + 4; })($n); echo $n;" "5")
      ("<?php $n=2; (fn(&$x)=>++$x)($n); echo $n;" "3"))
    "php-e2e-by-reference-parameters-literal-callables: ~S"
    (source expected)
    (expect (%php-run-capture source) :to-equal expected))

;; t/runtime-helpers-e2e-test.lisp:10 "php-e2e-foreach-over-array-literal"
(it-sequential
    "php-e2e-foreach-over-array-literal"
    (expect
      (%php-run-capture "<?php $s=0; foreach ([1,2,3] as $v) { $s=$s+$v; } echo $s;")
      :to-equal
      "6"))

;; t/runtime-helpers-e2e-test.lisp:16 "php-e2e-foreach-assoc-key-value"
(it-sequential
    "php-e2e-foreach-assoc-key-value"
    (expect
      (%php-run-capture
        "<?php $o=''; foreach (['a'=>1,'b'=>2,'c'=>3] as $k=>$v){ $o=$o.$k.'='.$v.';'; } echo $o;")
      :to-equal
      "a=1;b=2;c=3;"))

;; t/runtime-helpers-e2e-test.lisp:30 "php-e2e-by-reference-function-parameter"
(it-sequential
    "php-e2e-by-reference-function-parameter"
    (expect
      (%php-run-capture
        "<?php function inc(&$x) { $x=$x+1; } $n=1; inc($n); echo $n;")
      :to-equal
      "2"))

;; t/runtime-helpers-e2e-test.lisp:51 "php-e2e-generator-yield-foreach"
(it-sequential
    "php-e2e-generator-yield-foreach"
    (expect
      (%php-run-capture
        "<?php function g(){ yield 1; yield 2; yield 3; } $s=0; foreach (g() as $v){ $s=$s+$v; } echo $s;")
      :to-equal
      "6"))

;; t/runtime-helpers-e2e-test.lisp:58 "php-e2e-generator-parametrized"
(it-sequential
    "php-e2e-generator-parametrized"
    (expect
      (%php-run-capture
        "<?php function c($n){ for($i=1;$i<=$n;$i++){ yield $i; } } $s=0; foreach (c(5) as $v){ $s=$s+$v; } echo $s;")
      :to-equal
      "15"))

;; t/runtime-helpers-e2e-test.lisp:65 "php-e2e-generator-yield-from"
(it-sequential
    "php-e2e-generator-yield-from"
    (expect
      (%php-run-capture
        "<?php function inner(){ yield 1; yield 2; } function outer(){ yield 0; yield from inner(); yield 3; } $o=''; foreach (outer() as $v){ $o=$o.$v.','; } echo $o;")
      :to-equal
      "0,1,2,3,"))

;; t/runtime-helpers-e2e-test.lisp:72 "php-e2e-fiber-object-methods"
(it-sequential
    "php-e2e-fiber-object-methods"
    (expect
      (%php-run-capture
        "<?php $f = new Fiber(function(){ return 42; }); echo $f->start().':'.$f->getReturn().':'.($f->isTerminated()?1:0);")
      :to-equal
      "42:42:1"))

;; t/runtime-helpers-e2e-test.lisp:79 "php-e2e-fiber-suspend"
(it-sequential
    "php-e2e-fiber-suspend"
    (expect
      (%php-run-capture
        "<?php $f = new Fiber(function(){ return Fiber::suspend('pause'); }); echo $f->start().':'.($f->isSuspended()?1:0);")
      :to-equal
      "pause:1"))

;; t/runtime-helpers-e2e-test.lisp:86 "php-e2e-dynamic-closure-call"
(it-sequential
    "php-e2e-dynamic-closure-call"
    (expect
      (%php-run-capture "<?php $f = function($x){ return $x*10; }; echo $f(5);")
      :to-equal
      "50"))

;; t/runtime-helpers-e2e-test.lisp:92 "php-e2e-iife"
(it-sequential
    "php-e2e-iife"
    (expect
      (%php-run-capture "<?php echo (function($x){ return $x*2; })(5);")
      :to-equal
      "10"))

;; t/runtime-helpers-e2e-test.lisp:98 "php-e2e-closure-passed-to-user-function"
(it-sequential
    "php-e2e-closure-passed-to-user-function"
    (expect
      (%php-run-capture
        "<?php function apply($f,$v){ return $f($v); } echo apply(function($x){ return $x*3; }, 4);")
      :to-equal
      "12"))

;; t/runtime-helpers-php85-behavior-test.lisp:24 "PHP cast expressions execute through the runtime conversion helpers."
(it-sequential
    "PHP cast expressions execute through the runtime conversion helpers."
    (expect
      (%php-run-capture
        "<?php echo (int) '42' . ':' . (string) 7 . ':' . ((bool) 'x' ? 1 : 0);")
      :to-equal
      "42:7:1"))

;; t/runtime-helpers-php85-behavior-test.lisp:31 "PHP 8.5 deprecated cast spellings emit E_DEPRECATED while preserving cast results."
(it-sequential
    "PHP 8.5 deprecated cast spellings emit E_DEPRECATED while preserving cast results."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); echo (integer) '1'; echo '|'; echo (boolean) '1'; echo '|'; echo (double) '1.5'; echo '|'; echo (binary) 7; restore_error_handler();")
      :to-equal
      "8192:1|8192:1|8192:1.5|8192:7"))

;; t/runtime-helpers-php85-behavior-test.lisp:38 "Canonical cast spellings remain silent under PHP 8.5."
(it-sequential
    "Canonical cast spellings remain silent under PHP 8.5."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_DEPRECATED); echo (int) '1'; echo '|'; echo (bool) '1'; echo '|'; echo (float) '1.5'; echo '|'; echo (string) 7; restore_error_handler();")
      :to-equal
      "1|1|1.5|7"))

;; t/runtime-helpers-php85-behavior-test.lisp:45 "The @ operator suppresses PHP 8.5 non-canonical cast deprecation warnings."
(it-sequential
    "The @ operator suppresses PHP 8.5 non-canonical cast deprecation warnings."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_DEPRECATED); echo @(integer) '1'; restore_error_handler();")
      :to-equal
      "1"))

;; t/runtime-helpers-php85-behavior-test.lisp:52 "PHP 8.5 warns when switch case labels use a semicolon."
(it-sequential
    "PHP 8.5 warns when switch case labels use a semicolon."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); switch (1) { case 1; echo 'A'; break; } restore_error_handler();")
      :to-equal
      "8192:A"))

;; t/runtime-helpers-php85-behavior-test.lisp:59 "PHP 8.5 warns when switch default labels use a semicolon."
(it-sequential
    "PHP 8.5 warns when switch default labels use a semicolon."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); switch (0) { default; echo 'D'; } restore_error_handler();")
      :to-equal
      "8192:D"))

;; t/runtime-helpers-php85-behavior-test.lisp:66 "PHP 8.5 warns when __sleep() is used during serialization."
(it-sequential
    "PHP 8.5 warns when __sleep() is used during serialization."
    (expect
      (%php-run-capture
        "<?php class A { function __sleep(){ return ['x']; } public $x = 1; } function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); $o = new A; serialize($o); echo 'X'; restore_error_handler();")
      :to-equal
      "8192:X"))

;; t/runtime-helpers-php85-behavior-test.lisp:73 "PHP 8.5 warns when __wakeup() is used during unserialization."
(it-sequential
    "PHP 8.5 warns when __wakeup() is used during unserialization."
    (expect
      (%php-run-capture
        "<?php class A { function __wakeup(){} } function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); unserialize('O:1:\"A\":0:{}'); echo 'X'; restore_error_handler();")
      :to-equal
      "8192:X"))

;; t/runtime-helpers-php85-behavior-test.lisp:80 "PHP 8.5 warns when null is used as an array offset."
(it-sequential
    "PHP 8.5 warns when null is used as an array offset."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); $a = []; $a[null] = 1; echo 'X'; restore_error_handler();")
      :to-equal
      "8192:X"))

;; t/runtime-helpers-php85-behavior-test.lisp:87 "PHP 8.5 warns when null is used with array_key_exists()."
(it-sequential
    "PHP 8.5 warns when null is used with array_key_exists()."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); array_key_exists(null, ['' => 1]); echo 'X'; restore_error_handler();")
      :to-equal
      "8192:X"))

;; t/runtime-helpers-php85-behavior-test.lisp:94 "PHP 8.5 emits E_WARNING when list/[] destructuring reads a non-array value."
(it-sequential
    "PHP 8.5 emits E_WARNING when list/[] destructuring reads a non-array value."
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_WARNING); [$a] = false; echo $a === null ? 'N' : 'V'; restore_error_handler();")
      :to-equal
      "2:N")
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_WARNING); list($a) = 'x'; echo $a === null ? 'N' : 'V'; restore_error_handler();")
      :to-equal
      "2:N")
    (expect
      (%php-run-capture
        "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_WARNING); [$a] = null; echo $a === null ? 'N' : 'V'; restore_error_handler();")
      :to-equal
      "N"))

;; t/runtime-helpers-php85-behavior-test.lisp:111 "fatal_error_backtraces stays off unless explicitly enabled."
(it-sequential
    "fatal_error_backtraces stays off unless explicitly enabled."
    (multiple-value-bind (stdout stderr) (%php-run-capture-io "<?php $a = []; $b = $a[];")
      (declare (ignore stdout))
      (expect
        (search "PHP fatal error: Cannot use [] for reading" stderr :test #'char=)
        :to-be-truthy)
      (expect (search "VM backtrace:" stderr :test #'char=) :to-be-falsy)))

;; t/runtime-helpers-php85-behavior-test.lisp:119 "fatal_error_backtraces emits a VM backtrace for fatal errors."
(it-sequential
    "fatal_error_backtraces emits a VM backtrace for fatal errors."
    (multiple-value-bind (stdout stderr) (%php-run-capture-io
        "<?php ini_set('fatal_error_backtraces', '1'); function boom() { $a = []; $b = $a[]; } boom();")
      (declare (ignore stdout))
      (expect
        (search "PHP fatal error: Cannot use [] for reading" stderr :test #'char=)
        :to-be-truthy)
      (expect (search "VM backtrace:" stderr :test #'char=) :to-be-truthy)))

;; t/runtime-helpers-php85-behavior-test.lisp:128 "max_memory_limit cannot be changed at runtime."
(it-sequential
    "max_memory_limit cannot be changed at runtime."
    (multiple-value-bind (stdout stderr) (%php-run-capture-io
        "<?php echo ini_set('max_memory_limit', '64M') === false ? 'F' : 'T'; echo ':'; echo ini_get('max_memory_limit');"
        :ini-settings
        (%php-make-ini-settings "max_memory_limit" "128M"))
      (declare (ignore stderr))
      (expect stdout :to-equal "F:128M")))

;; t/runtime-helpers-php85-behavior-test.lisp:136 "memory_limit is clamped to max_memory_limit."
(it-sequential
    "memory_limit is clamped to max_memory_limit."
    (multiple-value-bind (stdout stderr) (%php-run-capture-io
        "<?php ini_set('memory_limit', '256M'); echo ini_get('memory_limit');"
        :ini-settings
        (%php-make-ini-settings "max_memory_limit" "128M"))
      (expect stdout :to-equal "128M")
      (expect (search "max_memory_limit" stderr :test #'char=) :to-be-truthy)))

;; t/runtime-helpers-php85-behavior-test.lisp:144 "memory_limit remains unconstrained when max_memory_limit is disabled."
(it-sequential
    "memory_limit remains unconstrained when max_memory_limit is disabled."
    (multiple-value-bind (stdout stderr) (%php-run-capture-io
        "<?php ini_set('memory_limit', '256M'); echo ini_get('memory_limit');"
        :ini-settings
        (%php-make-ini-settings "max_memory_limit" "-1"))
      (expect stdout :to-equal "256M")
      (expect (search "max_memory_limit" stderr :test #'char=) :to-be-falsy)))

;; t/runtime-helpers-php85-behavior-test.lisp:152 "PHP 8.5 permits scalar casts in constant expressions."
(it-sequential
    "PHP 8.5 permits scalar casts in constant expressions."
    (expect
      (%php-run-capture
        "<?php const I = (int) '42'; const S = (string) 7; const B = (bool) 'x'; echo I . ':' . S . ':' . (B ? 1 : 0);")
      :to-equal
      "42:7:1"))

;; t/runtime-helpers-php85-behavior-test.lisp:159 "PHP 8.5 clone($object) function-style syntax clones without overrides."
(it-sequential
    "PHP 8.5 clone($object) function-style syntax clones without overrides."
    (expect
      (%php-run-capture
        "<?php class C{ public $x=1; } $a=new C(); $b=clone($a); $b->x=2; echo $a->x.':'.$b->x;")
      :to-equal
      "1:2"))

;; t/runtime-helpers-php85-behavior-test.lisp:166 "Clone-with applies property overrides without mutating the original object."
(it-sequential
    "Clone-with applies property overrides without mutating the original object."
    (expect
      (%php-run-capture
        "<?php class C{ public $x=1; } $a=new C(); $b=clone($a, ['x'=>9]); echo $a->x.':'.$b->x;")
      :to-equal
      "1:9"))

;; t/runtime-helpers-php85-behavior-test.lisp:173 "Clone-with applies the override array after __clone has run."
(it-sequential
    "Clone-with applies the override array after __clone has run."
    (expect
      (%php-run-capture
        "<?php class C{ public $x; function __construct($x){ $this->x=$x; } function __clone(){ $this->x=$this->x+10; } } $a=new C(4); $b=clone($a, ['x'=>99]); echo $a->x.':'.$b->x;")
      :to-equal
      "4:99"))

;; t/runtime-helpers-php85-behavior-test.lisp:180 "Closure::getCurrent() signals Error outside closure execution."
(it-sequential
    "Closure::getCurrent() signals Error outside closure execution."
    (let ((condition
          (handler-case (progn
              (%php-run-capture "<?php Closure::getCurrent();")
              nil)
            (cl-cc/php:php-exception (e)
              e))))
      (expect condition :to-be-truthy)
      (expect (cl-cc/php:%php-exception-matches-p condition 'error) :to-be-truthy)
      (expect
        (cl-cc/php:%php-exception-value condition)
        :to-equal
        "Current function is not a closure.")))

;; t/runtime-helpers-php85-behavior-test.lisp:194 "Closure::getCurrent() returns the executing closure during direct invocation."
(it-sequential
    "Closure::getCurrent() returns the executing closure during direct invocation."
    (expect
      (%php-run-capture
        "<?php $f=function(){ return Closure::getCurrent(); }; echo $f() === $f ? 'same' : 'bad';")
      :to-equal
      "same"))

;; t/runtime-helpers-php85-behavior-test.lisp:201 "Closure::getCurrent() returns the executing closure through call_user_func()."
(it-sequential
    "Closure::getCurrent() returns the executing closure through call_user_func()."
    (expect
      (%php-run-capture
        "<?php $f=function(){ return Closure::getCurrent(); }; echo call_user_func($f) === $f ? 'same' : 'bad';")
      :to-equal
      "same"))

;; t/runtime-helpers-php85-behavior-test.lisp:295 "flock() supports exclusive locking, unlocking, and reacquiring in the CLI model."
(it-sequential
    "flock() supports exclusive locking, unlocking, and reacquiring in the CLI model."
    (let* ((tmp-dir (uiop:temporary-directory))
           (path (cl-cc/php::%php-tempnam tmp-dir "cl-cc-php85-flock-"))
           (php-path (namestring path)))
      (unwind-protect (progn
          (with-open-file (stream
              path
              :direction
              :output
              :if-exists
              :supersede
              :if-does-not-exist
              :create)
            (write-string "lock me" stream))
          (expect
            (%php-run-capture
              (format
                nil
                "<?php
$fp1 = fopen('~A', 'r+');
$fp2 = fopen('~A', 'r+');
$first = flock($fp1, LOCK_EX);
$blocked = flock($fp2, LOCK_EX | LOCK_NB);
$released = flock($fp1, LOCK_UN);
$second = flock($fp2, LOCK_EX | LOCK_NB);
echo ($first ? 'Y' : 'N') . ':' .
     ($blocked ? 'Y' : 'N') . ':' .
     ($released ? 'Y' : 'N') . ':' .
     ($second ? 'Y' : 'N');"
                php-path
                php-path))
            :to-equal
            "Y:N:Y:Y"))
        (ignore-errors (delete-file path)))))

;; t/runtime-helpers-php85-behavior-test.lisp:366 "isset() evaluates through the syntax lowering path without requiring the variable to exist first."
(it-sequential
    "isset() evaluates through the syntax lowering path without requiring the variable to exist first."
    (expect
      (%php-run-capture "<?php echo isset($missing) ? 'T' : 'F';")
      :to-equal
      "F"))

;; t/runtime-helpers-php85-behavior-test.lisp:372 "empty() evaluates through the syntax lowering path when the variable is known."
(it-sequential
    "empty() evaluates through the syntax lowering path when the variable is known."
    (expect
      (%php-run-capture "<?php $x = 0; echo empty($x) ? 'T' : 'F';")
      :to-equal
      "T"))

;; t/runtime-helpers-php85-behavior-test.lisp:378 "compact() captures a visible variable through the parser lowering path."
(it-sequential
    "compact() captures a visible variable through the parser lowering path."
    (expect
      (%php-run-capture "<?php $x = 42; $a = compact('x'); echo $a['x'];")
      :to-equal
      "42"))

;; t/runtime-helpers-php85-behavior-test.lisp:384 "extract() introduces variables through the parser lowering path."
(it-sequential
    "extract() introduces variables through the parser lowering path."
    (expect
      (%php-run-capture "<?php extract(['x' => 17]); echo $x;")
      :to-equal
      "17"))

;; t/runtime-helpers-php85-behavior-test.lisp:390 "unset() removes an array slot through the parser lowering path."
(it-sequential
    "unset() removes an array slot through the parser lowering path."
    (expect
      (%php-run-capture
        "<?php $a = ['x' => 1]; unset($a['x']); echo isset($a['x']) ? 'T' : 'F';")
      :to-equal
      "F"))

;; t/runtime-helpers-php85-behavior-test.lisp:397 "unset() clears object properties through the parser lowering path."
(it-sequential
    "unset() clears object properties through the parser lowering path."
    (expect
      (%php-run-capture
        "<?php $o = new class { public $x = 1; }; unset($o->x); echo isset($o->x) ? 'T' : 'F';")
      :to-equal
      "F"))
