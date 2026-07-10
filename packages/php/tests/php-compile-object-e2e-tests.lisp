(in-package :cl-cc/test)

(in-suite cl-cc-php-e2e-suite)

(deftest php-e2e-static-members
  "Static methods and properties resolve via ClassName::member.  Regression: the
`static` modifier lexes as a T-TYPE (it doubles as the `static` return type), so
the class-body modifier parser skipped it — static props/methods were left
instance-allocated and ClassName::member found nothing on the class object
('Undefined function: NIL' / empty).  Static members are now class-allocated
(like class constants), so slot-value on the class object resolves them."
  ;; static method called by class name
  (assert-string= "25" (%php-run-capture "<?php class M{ static function sq($x){ return $x*$x; } } echo M::sq(5);"))
  ;; static property read
  (assert-string= "5"  (%php-run-capture "<?php class C{ static $n=5; } echo C::$n;"))
  ;; static property write paths share the same slot-value target lowering as
  ;; instance properties.
  (assert-string= "7"  (%php-run-capture "<?php class C{ static $n=1; } C::$n=7; echo C::$n;"))
  (assert-string= "7"  (%php-run-capture "<?php class C{ static $n=2; } C::$n += 5; echo C::$n;"))
  (assert-string= "1:2" (%php-run-capture "<?php class C{ static $n=1; } echo C::$n++; echo ':'.C::$n;"))
  (assert-string= "2:2" (%php-run-capture "<?php class C{ static $n=1; } echo ++C::$n; echo ':'.C::$n;"))
  (assert-string= "3:3" (%php-run-capture "<?php class C{ static $n=1; static function inc(){ self::$n += 2; return self::$n; } } echo C::inc().':'.C::$n;"))
  ;; class constant still works alongside a static property in the same class
  (assert-string= "3|5" (%php-run-capture "<?php class C{ const PI=3; static $n=5; } echo C::PI.'|'.C::$n;"))
  ;; one static method consuming another's result, both by class name
  (assert-string= "21" (%php-run-capture "<?php class U{ static function inc($x){ return $x+1; } static function dbl($x){ return $x*2; } } echo U::inc(U::dbl(10));")))


(deftest php-e2e-self-static-parent
  "self::, static::, and parent:: resolve inside method bodies, including a class
referring to itself for recursion (ClassName::m / self::m).  Regression: methods
were compiled as class-slot initforms BEFORE the class registered its own global,
so a self-reference hit the 'Unbound variable' path and the whole class form was
dropped (silent empty output).  The class name is now registered as a global
before its method bodies compile."
  ;; self:: static method dispatch
  (assert-string= "9"  (%php-run-capture "<?php class C{ static function f(){ return self::g(); } static function g(){ return 9; } } echo C::f();"))
  ;; static:: (late static binding spelling) dispatch
  (assert-string= "7"  (%php-run-capture "<?php class C{ static function f(){ return static::g(); } static function g(){ return 7; } } echo C::f();"))
  ;; self::CONST inside a method
  (assert-string= "42" (%php-run-capture "<?php class C{ const X=42; static function get(){ return self::X; } } echo C::get();"))
  ;; recursion via ClassName::m
  (assert-string= "120" (%php-run-capture "<?php class F{ static function fac($n){ return $n<=1?1:$n*F::fac($n-1); } } echo F::fac(5);"))
  ;; recursion via self::m
  (assert-string= "720" (%php-run-capture "<?php class F{ static function fac($n){ return $n<=1?1:$n*self::fac($n-1); } } echo F::fac(6);"))
  ;; parent:: static dispatch to a superclass method
  (assert-string= "11" (%php-run-capture "<?php class A{ static function who(){ return 1; } } class B extends A{ static function who2(){ return parent::who()+10; } } echo B::who2();"))
  ;; a static method called from inside an instance method, by class name
  (assert-string= "109" (%php-run-capture "<?php class K{ public $base=100; static function tag(){ return 9; } function show(){ return $this->base+K::tag(); } } $o=new K(); echo $o->show();")))


(deftest php-e2e-constructor
  "new C(args) runs __construct with the instance as $this and the args
(regression: the constructor never ran — args were passed as :ARGn CLOS initargs
the class rejected). A class with no __construct is unaffected."
  (assert-string= "made" (%php-run-capture "<?php class C{ function __construct(){ echo 'made'; } } $o=new C();"))
  (assert-string= "9"    (%php-run-capture "<?php class C{ public $x; function __construct($v){ $this->x=$v; } } $o=new C(9); echo $o->x;"))
  (assert-string= "Bob:30" (%php-run-capture "<?php class P{ public $n; public $a; function __construct($n,$a){ $this->n=$n; $this->a=$a; } } $p=new P('Bob',30); echo $p->n.':'.$p->a;"))
  ;; constructor + a method that uses the constructed state
  (assert-string= "10"   (%php-run-capture "<?php class C{ public $x; function __construct($v){ $this->x=$v; } function dbl(){ return $this->x*2; } } $o=new C(5); echo $o->dbl();"))
  ;; a class with no constructor still constructs and uses property defaults
  (assert-string= "5"    (%php-run-capture "<?php class C{ public $x=5; } $o=new C(); echo $o->x;")))


(deftest php-e2e-clone-object
  "clone makes a shallow object copy and invokes __clone on the copied instance."
  (assert-string= "1:9"
    (%php-run-capture "<?php class C{ public $x=1; } $a=new C(); $b=clone $a; $b->x=9; echo $a->x.':'.$b->x;"))
  (assert-string= "4:14"
    (%php-run-capture "<?php class C{ public $x; function __construct($x){ $this->x=$x; } function __clone(){ $this->x=$this->x+10; } } $a=new C(4); $b=clone $a; echo $a->x.':'.$b->x;")))


(deftest php-e2e-constructor-promotion
  "PHP 8.0 constructor property promotion: `public $x` in __construct parameter
list auto-assigns $this->x = $x without explicit body code."
  ;; basic single-param promotion
  (assert-string= "42"
    (%php-run-capture "<?php class C{ function __construct(public $x){} } $o=new C(42); echo $o->x;"))
  ;; two promoted params
  (assert-string= "hello:99"
    (%php-run-capture "<?php class P{ function __construct(public $n, public $v){} } $p=new P('hello',99); echo $p->n.':'.$p->v;"))
  ;; mixed: one promoted, one regular param — $this->x auto-set, $label used in body
  (assert-string= "5:extra"
    (%php-run-capture "<?php class M{ function __construct(public $x, $label){ echo $this->x.':'.$label; } } $o=new M(5,'extra');"))
  ;; promotion + explicit body
  (assert-string= "10"
    (%php-run-capture "<?php class C{ function __construct(public $v){ $this->v=$this->v*2; } } $o=new C(5); echo $o->v;")))


(deftest php-e2e-instance-method-this
  "Instance methods bind $this to the receiver (regression: $this was an unbound
variable, so $this->x inside a method produced nothing). Implemented by giving
each instance method an implicit $this first parameter that the call site
($o->m(args)) fills with the receiver."
  (assert-string= "3"  (%php-run-capture "<?php class C{ public $x=3; function g(){ return $this->x; } } $o=new C(); echo $o->g();"))
  (assert-string= "15" (%php-run-capture "<?php class C{ public $b=10; function add($a){ return $this->b+$a; } } $o=new C(); echo $o->add(5);"))
  (assert-string= "7"  (%php-run-capture "<?php class C{ public $n=0; function setN($v){ $this->n=$v; } } $o=new C(); $o->setN(7); echo $o->n;"))
  (assert-string= "2"  (%php-run-capture "<?php class C{ public $c=0; function inc(){ $this->c=$this->c+1; return $this->c; } } $o=new C(); $o->inc(); echo $o->inc();"))
  ;; returning $this and chaining method calls
  (assert-string= "1"  (%php-run-capture "<?php class C{ public $v=1; function get(){ return $this->v; } function me(){ return $this; } } $o=new C(); echo $o->me()->get();"))
  ;; a method that does not use $this still works (receiver passed but unused)
  (assert-string= "42" (%php-run-capture "<?php class C{ function g(){ return 42; } } $o=new C(); echo $o->g();")))


(deftest php-e2e-class-property-access
  "Public properties read/write through $o->x with the correct slot name
(regression: a property declared via php-var-sym got slot |x| while $o->x looked
up X -> 'slot X is missing'), and a `= default' initializes the slot."
  (assert-string= "7" (%php-run-capture "<?php class C{ public $x=7; } $o=new C(); echo $o->x;"))
  (assert-string= "5" (%php-run-capture "<?php class C{ public $x; } $o=new C(); $o->x=5; echo $o->x;"))
  (assert-string= "x=7" (%php-run-capture "<?php class C{ public $x=7; } $o=new C(); echo \"x={$o->x}\";"))
  (assert-string= "hi" (%php-run-capture "<?php class C{ public $s='hi'; } $o=new C(); echo $o->s;")))


(deftest php-runtime-enum-helpers
  "PHP enum runtime helpers return cases and backed lookups with PHP null fallback."
  (let* ((class (make-hash-table :test #'eq))
         (draft (cl-cc/php:%php-enum-make-case 'status 'draft 0))
         (published (cl-cc/php:%php-enum-make-case 'status 'published 1)))
    (setf (gethash :__class-slots__ class) '(draft published)
          (gethash 'draft class) draft
          (gethash 'published class) published)
    ;; %php-enum-case-list is the internal CL list; %php-enum-cases (ENUM::cases())
    ;; now returns a PHP array (hash-table) so count()/foreach work.
    (assert-equal (list draft published) (cl-cc/php:%php-enum-case-list class))
    (assert-true (hash-table-p (cl-cc/php:%php-enum-cases class)))
    (assert-= 2 (cl-cc/php:%php-count (cl-cc/php:%php-enum-cases class)))
    (assert-eq published (cl-cc/php:%php-enum-from class 1))
    (assert-equal cl-cc/php:+php-null+ (cl-cc/php:%php-enum-try-from class 99))
    (assert-= 1 (cl-cc/php:%php-enum-case-value published))))


(deftest php-compile-enum-static-builtins
  "PHP enums compile through static case and from/tryFrom/cases helper lowering."
  (let ((result (cl-cc:compile-string
                 "<?php enum Status: int { case Draft = 0; case Published = 1; } $a = Status::Published; $b = Status::from(1); $c = Status::tryFrom(99); $d = Status::cases();"
                 :target :vm
                 :language :php)))
    (assert-true (typep result 'cl-cc/compile:compilation-result))))


(deftest php-e2e-uninitialized-property-null
  "An untyped property declared without an initializer (public $x;) defaults to
PHP null, so is_null($o->x) is true and $o->x ?? d coalesces.  Regression: the
slot read as the unbound-slot-marker, so is_null was false and ?? did not fall
through to the default."
  (assert-string= "yes" (%php-run-capture "<?php class C{public $x;} $o=new C(); echo is_null($o->x)?'yes':'no';"))
  (assert-string= "def" (%php-run-capture "<?php class C{public $x;} $o=new C(); echo $o->x ?? 'def';"))
  ;; an explicit default is unaffected
  (assert-string= "7"   (%php-run-capture "<?php class C{public $x=7;} $o=new C(); echo $o->x ?? 'def';"))
  (assert-string= "hi"  (%php-run-capture "<?php class C{public $s='hi';} $o=new C(); echo $o->s;"))
  ;; writing then reading a no-default property still works
  (assert-string= "9"   (%php-run-capture "<?php class C{public $x;} $o=new C(); $o->x=9; echo $o->x;")))


(deftest php-e2e-unset-object-property
  "unset($o->x) resets the property to PHP null in the current object model."
  (assert-string= "null"
                  (%php-run-capture "<?php class C{public $x=7;} $o=new C(); unset($o->x); echo is_null($o->x)?'null':'set';"))
  (assert-string= "D"
                  (%php-run-capture "<?php class C{public $x='x';} $o=new C(); unset($o->x); echo $o->x ?? 'D';"))
  (assert-string= "1:null:0"
                  (%php-run-capture "<?php class C{public $x=1; public $y=2;} $o=new C(); $a=[9]; unset($a[0], $o->y); echo $o->x.':'.(is_null($o->y)?'null':'set').':'.count($a);")))


(deftest php-e2e-enum-name-value-cases
  "Enum cases expose ->name and ->value, S::cases() is a PHP array, and
S::from/tryFrom resolve backed cases.  Regression: cases stored name/value under
CL symbol keys in an EQ hash-table, so $case->name (a string-key fallback) never
matched; cases() returned a CL list that broke count()/foreach."
  (assert-string= "A"  (%php-run-capture "<?php enum S{case A;} echo S::A->name;"))
  (assert-string= "a"  (%php-run-capture "<?php enum S:string{case A='a';case B='b';} echo S::A->value;"))
  (assert-string= "10" (%php-run-capture "<?php enum S:int{case A=10;} echo S::A->value;"))
  (assert-string= "2"  (%php-run-capture "<?php enum S{case A;case B;} echo count(S::cases());"))
  (assert-string= "AB" (%php-run-capture "<?php enum S{case A;case B;} $n=''; foreach(S::cases() as $c){$n.=$c->name;} echo $n;"))
  (assert-string= "B"  (%php-run-capture "<?php enum S:int{case A=1;case B=2;} echo S::from(2)->name;"))
  (assert-string= "null" (%php-run-capture "<?php enum S:int{case A=1;} echo S::tryFrom(9)===null?'null':'f';"))
  (assert-string= "y"  (%php-run-capture "<?php enum S{case A;} echo S::A===S::A?'y':'n';"))
  ;; match on enum cases
  (assert-string= "hearts" (%php-run-capture "<?php enum Suit:string{case H='h';case S='s';} $x=Suit::H; echo match($x){Suit::H=>'hearts',Suit::S=>'spades'};")))


(deftest php-e2e-enum-methods
  "Enum cases can call methods defined on the enum, with $this bound to the case.
Regression: enum methods lived on the class as instance-method slots that the
case singletons (plain payloads, not make-instance objects) never received, so
S::A->m() found no method ('Undefined function: NIL').  Enum methods are now
class-allocated and each case is linked to the enum class via __class__."
  (assert-string= "L" (%php-run-capture "<?php enum S{case A; public function label(){return 'L';}} echo S::A->label();"))
  ;; $this->value inside a method
  (assert-string= "A" (%php-run-capture "<?php enum S:string{case A='a'; public function up(){return strtoupper($this->value);}} echo S::A->up();"))
  ;; match($this) inside a method
  (assert-string= "red" (%php-run-capture "<?php enum Suit{case H;case S; public function color(){return match($this){Suit::H=>'red',Suit::S=>'black'};}} echo Suit::H->color();"))
  ;; method with an argument
  (assert-string= "6" (%php-run-capture "<?php enum S{case A; public function add($n){return $n+1;}} echo S::A->add(5);"))
  ;; methods coexist with name/value/const and don't break them
  (assert-string= "A" (%php-run-capture "<?php enum S{case A; public function x(){return 1;}} echo S::A->name;"))
  (assert-string= "5" (%php-run-capture "<?php enum S{case A; const X=5; public function y(){return 2;}} echo S::X;")))


(deftest php-e2e-nullsafe-operator
  "The ?-> nullsafe operator short-circuits to null when the receiver is null,
else reads the property / calls the method (passing the receiver as \$this).
Regression: the null check was an (ast-binop := ...) — CL NUMERIC equality — so
\$o?->x on an object raised 'not of type NUMBER'; the receiver was also evaluated
twice and ?->m() did not pass \$this."
  (assert-string= "5"       (%php-run-capture "<?php class C{public $x=5;} $o=new C(); echo $o?->x;"))
  (assert-string= "def"     (%php-run-capture "<?php class C{public $x=5;} $o=null; echo $o?->x ?? 'def';"))
  (assert-string= "7"       (%php-run-capture "<?php class C{function m(){return 7;}} $o=new C(); echo $o?->m();"))
  (assert-string= "n"       (%php-run-capture "<?php class C{function m(){return 7;}} $o=null; echo $o?->m() ?? 'n';"))
  ;; ?-> method passes the receiver as $this
  (assert-string= "3"       (%php-run-capture "<?php class C{public $v=3; function get(){return $this->v;}} $o=new C(); echo $o?->get();"))
  ;; chaining: a null link short-circuits the rest
  (assert-string= "none"    (%php-run-capture "<?php class C{public $n=null;} $o=new C(); echo $o?->n?->x ?? 'none';"))
  ;; a null property value coalesces
  (assert-string= "wasnull" (%php-run-capture "<?php class C{public $x;} $o=new C(); echo $o?->x ?? 'wasnull';")))


(deftest php-e2e-predefined-constants
  "Predefined PHP constants (PHP_EOL, PHP_INT_MAX, M_PI, STR_PAD_LEFT, SORT_*).
A bare identifier not followed by '(' was lowered to an ast-var — an undefined
global — so every constant read as the empty string.  Now php-parse-primary
consults *php-predefined-constants* and lowers a hit to its literal value."
  (assert-string= "9223372036854775807" (%php-run-capture "<?php echo PHP_INT_MAX;"))
  (assert-string= "8"       (%php-run-capture "<?php echo PHP_INT_SIZE;"))
  (assert-string= "3.14159" (%php-run-capture "<?php echo round(M_PI,5);"))
  (assert-string= "8.5.0"   (%php-run-capture "<?php echo PHP_VERSION;"))
  (assert-string= "8.5.0"   (%php-run-capture "<?php echo phpversion();"))
  (assert-string= "1"       (%php-run-capture "<?php echo is_nan(NAN) ? '1' : '0';"))
  (assert-string= "2"       (%php-run-capture "<?php echo SORT_STRING;"))
  ;; PHP_EOL is a real newline
  (assert-string= "y" (%php-run-capture "<?php echo PHP_EOL===\"\\n\"?'y':'n';"))
  ;; STR_PAD_LEFT/RIGHT drive str_pad correctly (were empty -> broken padding)
  (assert-string= "005" (%php-run-capture "<?php echo str_pad('5',3,'0',STR_PAD_LEFT);"))
  (assert-string= "500" (%php-run-capture "<?php echo str_pad('5',3,'0',STR_PAD_RIGHT);"))
  ;; leading-backslash qualified reference resolves to the global constant
  (assert-string= "8" (%php-run-capture "<?php echo \\PHP_MAJOR_VERSION;"))
  ;; an UNKNOWN bare identifier still lowers to a var (no crash, empty value)
  (assert-string= "" (%php-run-capture "<?php echo NOT_A_REAL_CONSTANT_XYZ;")))


(deftest php-e2e-predefined-constants-are-not-functions
  "Predefined constants resolve through the constant table, not the builtin
function registry."
  (assert-string= "absent:absent:absent"
                  (%php-run-capture
                   "<?php echo function_exists('PHP_EOL')?'present':'absent'; echo ':'; echo function_exists('SORT_STRING')?'present':'absent'; echo ':'; echo function_exists('null')?'present':'absent';")))


(deftest php-e2e-call-user-func
  "call_user_func / call_user_func_array over Closure, builtin-name, and
USER-function-name callables.  String callables for USER functions previously
failed ('Invalid function designator') because the resolver only checked the
builtin registry; now %php-callable-user-function matches the VM function registry
by package-independent SYMBOL-NAME."
  ;; user function by string name
  (assert-string= "25" (%php-run-capture "<?php function sq($x){return $x*$x;} echo call_user_func('sq',5);"))
  (assert-string= "7"  (%php-run-capture "<?php function add($a,$b){return $a+$b;} echo call_user_func('add',3,4);"))
  ;; builtin by string name
  (assert-string= "HI" (%php-run-capture "<?php echo call_user_func('strtoupper','hi');"))
  ;; closure / arrow
  (assert-string= "101" (%php-run-capture "<?php $f=function($n){return $n+100;}; echo call_user_func($f,1);"))
  (assert-string= "21"  (%php-run-capture "<?php $g=fn($x)=>$x*3; echo call_user_func($g,7);"))
  ;; call_user_func_array spreads the array as positional args
  (assert-string= "30" (%php-run-capture "<?php function add($a,$b){return $a+$b;} echo call_user_func_array('add',[10,20]);"))
  (assert-string= "36" (%php-run-capture "<?php function sq($x){return $x*$x;} echo call_user_func_array('sq',[6]);")))


(deftest php-e2e-spl-autoload-default-loader-is-absent
  "spl_autoload() is not exposed until include-path backed class loading exists."
  (assert-string= "absent"
                  (%php-run-capture
                   "<?php echo function_exists('spl_autoload')?'present':'absent';")))


(deftest php-e2e-spl-autoload-registry
  "SPL autoload registration keeps PHP-visible callback state."
  (assert-string= "RP:2:clcc_loader_b:clcc_loader_a:U:clcc_loader_b,:missing"
                  (%php-run-capture
                   "<?php function clcc_loader_a($c){} function clcc_loader_b($c){} spl_autoload_unregister('clcc_loader_a'); spl_autoload_unregister('clcc_loader_b'); echo spl_autoload_register('clcc_loader_a')?'R':'F'; echo spl_autoload_register('clcc_loader_b', true, true)?'P':'F'; $list=spl_autoload_functions(); echo ':'.count($list).':'.$list[0].':'.$list[1]; echo ':'.(spl_autoload_unregister('clcc_loader_a')?'U':'N'); $list=spl_autoload_functions(); $found=''; foreach($list as $fn){$found.=$fn.',';} echo ':'.$found; echo ':'.(spl_autoload_unregister('clcc_loader_a')?'again':'missing'); spl_autoload_unregister('clcc_loader_b');")))


(deftest php-e2e-class-reflection-builtins
  "class_implements/class_parents/class_uses reflect compiler class metadata
as PHP-visible arrays."
  (assert-string= "REFLIFACEBREFLIFACEI"
                  (%php-run-capture
                   "<?php interface ReflIfaceI{} interface ReflIfaceB extends ReflIfaceI{} class ReflImpl implements ReflIfaceB{} $s=''; foreach(class_implements(new ReflImpl()) as $v){$s.=$v;} echo $s;"))
  (assert-string= "REFLMIDREFLBASE"
                  (%php-run-capture
                   "<?php class ReflBase{} class ReflMid extends ReflBase{} class ReflLeaf extends ReflMid{} $s=''; foreach(class_parents(new ReflLeaf()) as $v){$s.=$v;} echo $s;"))
  (assert-string= "REFLSTRINGBASE"
                  (%php-run-capture
                   "<?php class ReflStringBase{} class ReflStringChild extends ReflStringBase{} $s=''; foreach(class_parents('ReflStringChild') as $v){$s.=$v;} echo $s;"))
  (assert-string= "REFLTRAITTREFLTRAITU"
                  (%php-run-capture
                   "<?php trait ReflTraitT{} trait ReflTraitU{} class ReflTraitUser{use ReflTraitT,ReflTraitU;} $s=''; foreach(class_uses(new ReflTraitUser()) as $v){$s.=$v;} echo $s;")))


(deftest php-e2e-spl-data-structures
  "SPL data structures created with new expose stateful methods."
  (assert-string= "ba1SplStackY"
                  (%php-run-capture
                   "<?php $s=new SplStack(); $s->push('a'); $s->push('b'); echo $s->pop().$s->top().$s->count().get_class($s); echo method_exists($s,'push')?'Y':'N';"))
  (assert-string= "ab1"
                  (%php-run-capture
                   "<?php $q=new SplQueue(); $q->enqueue('a'); $q->enqueue('b'); echo $q->dequeue().$q->bottom().$q->count();"))
  (assert-string= "xy3z"
                  (%php-run-capture
                   "<?php $f=new SplFixedArray(2); $f->offsetSet(0,'x'); $f->offsetSet(1,'y'); $f->setSize(3); $f->offsetSet(2,'z'); echo $f->offsetGet(0).$f->offsetGet(1).$f->getSize().$f->offsetGet(2);"))
  (assert-string= "13"
                  (%php-run-capture
                   "<?php $min=new SplMinHeap(); $max=new SplMaxHeap(); foreach([3,1,2] as $v){$min->insert($v);$max->insert($v);} echo $min->extract().$max->extract();")))
