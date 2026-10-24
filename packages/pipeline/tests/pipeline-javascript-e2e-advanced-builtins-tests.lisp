;;;; t/e2e-advanced-builtins-test.lisp — JS advanced execution tests, part 2
;;;;
;;;; Split from e2e-advanced-test.lisp once it passed the org's 500-line cap:
;;;; class features (fields, super, statics), contextual keywords/Symbols/
;;;; global functions, for-in/of loops, generators, Map/Set/Array.from, and
;;;; every builtin static-method family (Math, Number, Error hierarchy,
;;;; String, Array.of, old-style function constructors, Object/Reflect
;;;; reflection, BigInt).
;;;;
;;;; Depends on: e2e-core-test.lisp (%js-run-capture, deftest-js-run).

(in-package :cl-cc/test)

;;; ─── Class features ──────────────────────────────────────────────────────────

(deftest-js-run js-e2e-super-constructor
  "super(args) calls the parent constructor; multi-level chains call the right parent."
  ("y"    "class A{constructor(n){this.n=n;}} class B extends A{constructor(n){super(n);}} console.log(new B('y').n);")
  ("x x!" "class A{constructor(n){this.n=n;}} class B extends A{constructor(n){super(n);this.m=n+'!';}} const b=new B('x'); console.log(b.n+' '+b.m);")
  ("11"   "class A{constructor(){this.k=1;}} class B extends A{constructor(){super();this.k+=10;}} console.log(new B().k);")
  ("12"   "class A{constructor(x){this.x=x;}} class B extends A{constructor(x){super(x*2);}} class C extends B{constructor(x){super(x+1);}} console.log(new C(5).x);")
  ("z"    "class A{constructor(n){this.n=n;}} class B extends A{} console.log(new B('z').n);"))

(deftest-js-run-isolated-batch js-e2e-class-fields
  "Class field initializers run per-instance before constructor body; fields can reference each other."
  ("0"    "class C{count=0;} console.log(new C().count);")
  ("15"   "class C{x=5;y=10;} const c=new C(); console.log(c.x+c.y);")
  ("hi"   "class C{name='hi';} console.log(new C().name);")
  ("1,2"  "class C{items=[];add(v){this.items.push(v);return this.items.length;}} const c=new C(); console.log(c.add(1)+','+c.add(2));")
  ("11"   "class C{x=1;constructor(){this.x+=10;}} console.log(new C().x);")
  ("6"    "class C{a=2;b=this.a*3;} console.log(new C().b);")
  ("y 99" "class A{constructor(n){this.n=n;}} class D extends A{extra=99;} const d=new D('y'); console.log(d.n+' '+d.extra);"))

(deftest-js-run-isolated-batch js-e2e-class-self-reference
  "A class can reference itself inside its own methods (static create, clone, recursive)."
  ("z"      "class A{constructor(n){this.n=n;} static create(n){return new A(n);}} console.log(A.create('z').n);")
  ("6"      "class A{constructor(n){this.n=n;} clone(){return new A(this.n+1);}} console.log(new A(5).clone().n);")
  ("true"   "class A{static self(){return A;}} console.log(A.self()===A);")
  ("42"     "class A{static y(){return 42;} static z(){return A.y();}} console.log(A.z());")
  ("object" "class A{static make(){return new A();}} console.log(typeof A.make());"))

(deftest-js-run js-e2e-static-fields
  "static field initializers set the value on the class object; case is preserved."
  ("1.0" "class A{static VERSION='1.0';} console.log(A.VERSION);")
  ("100" "class A{static MAX=100; static MIN=0;} console.log(A.MAX-A.MIN);")
  ("1 2" "class A{static count=0; static inc(){return ++A.count;}} console.log(A.inc()+' '+A.inc());")
  ("3"   "class A{static items=[1,2,3];} console.log(A.items.length);")
  ("7"   "class C{COUNT=7;} console.log(new C().COUNT);"))

(deftest-js-run js-e2e-case-sensitive-identifiers
  "Identifiers are case-sensitive: a/A, foo/Foo are distinct."
  ("5 10" "const A=5; const a=10; console.log(A+' '+a);")
  ("1 2"  "function foo(){return 1;} function Foo(){return 2;} console.log(foo()+' '+Foo());")
  ("10"   "class A{static Y=10;m(){return 1;}} const a=new A(); a.m(); console.log(A.Y);")
  ("7 0"  "class Point{constructor(x){this.x=x;} static origin(){return new Point(0);}} const p=new Point(7); console.log(p.x+' '+Point.origin().x);"))

(deftest-js-run-isolated-batch js-e2e-super-method
  "super.method() calls the parent method; super() constructor calls still work in same class."
  ("11"      "class A{f(){return 1;}} class B extends A{f(){return super.f()+10;}} console.log(new B().f());")
  ("AB"      "class A{greet(){return 'A';}} class B extends A{greet(){return super.greet()+'B';}} console.log(new B().greet());")
  ("10"      "class A{constructor(){this.x=5;} m(){return this.x;}} class B extends A{m(){return super.m()*2;}} console.log(new B().m());")
  ("Hi Bob!" "class A{constructor(n){this.n=n;} greet(){return 'Hi '+this.n;}} class B extends A{constructor(n){super(n);} greet(){return super.greet()+'!';}} console.log(new B('Bob').greet());")
  ("k"       "class A{constructor(n){this.n=n;}} class B extends A{constructor(n){super(n);}} console.log(new B('k').n);"))

;;; ─── Contextual keywords, Symbols, global functions ─────────────────────────

(deftest-js-run js-e2e-contextual-keywords-as-identifiers
  "Contextual keywords (get/set/of/from/static) are valid in binding positions."
  ("5"  "const set=5; console.log(set);")
  ("3"  "const of=1, from=2; console.log(of+from);")
  ("10" "function f(set){return set*2;} console.log(f(5));")
  ("3"  "const [get,set]=[1,2]; console.log(get+set);")
  ("5"  "class C{get x(){return 5;}} console.log(new C().x);")
  ("6"  "let s=0; for(const x of [1,2,3]){s+=x;} console.log(s);"))

(deftest-js-run-isolated-batch js-e2e-symbol-constructor
  "Symbol(desc) is callable; .description is an accessor property; computed keys work."
  ("symbol"    "console.log(typeof Symbol('x'));")
  ("Symbol(d)" "console.log(Symbol('d').toString());")
  ("k"         "console.log(Symbol('k').description);")
  ("true"      "console.log(Symbol().description===undefined);")
  ("symbol"    "console.log(typeof Symbol());")
  ("42"        "const o={}; const k=Symbol('key'); o[k]=42; console.log(o[k]);")
  ("symbol"    "console.log(typeof Symbol.iterator);")
  ("true"      "const s=Symbol('v'); console.log(s.valueOf()===s);"))

(deftest-js-run-isolated-batch js-e2e-global-function-calls
  "btoa/atob/encode*/decode*/isNaN/isFinite are callable as bare global calls."
  ("aGk="      "console.log(btoa('hi'));")
  ("hi"        "console.log(atob('aGk='));")
  ("a%20b"     "console.log(encodeURIComponent('a b'));")
  ("a b"       "console.log(decodeURIComponent('a%20b'));")
  ("a%20b/c"   "console.log(encodeURI('a b/c'));")
  ("false true" "console.log(isNaN(5)+' '+isNaN(NaN));")
  ("true false" "console.log(isFinite(5)+' '+isFinite(Infinity));"))

;;; ─── for-in / for-of loops ───────────────────────────────────────────────────

(deftest-js-run js-e2e-for-in
  "for...in enumerates enumerable string keys of an object."
  ("a,b,c" "const o={a:1,b:2,c:3}; const keys=[]; for(let k in o){keys.push(k);} console.log(keys.join(','));")
  ("x"     "const o={x:10}; let result=''; for(let k in o){result+=k;} console.log(result);")
  ;; Array-index keys must enumerate numerically ascending, ahead of every
  ;; other key regardless of insertion order (ES2015+ [[OwnPropertyKeys]] --
  ;; same rule Object.keys applies, checked separately in
  ;; js-e2e-object-static-methods).
  ("1,2,foo" "const o={2:'b',foo:'bar',1:'a'}; const keys=[]; for(let k in o){keys.push(k);} console.log(keys.join(','));"))

(deftest-js-run js-e2e-for-of-array
  "for...of iterates elements of an array."
  ("1,2,3" "const a=[1,2,3]; const out=[]; for(let v of a){out.push(v);} console.log(out.join(','));")
  ("30"    "let s=0; for(const n of [10,20]){s+=n;} console.log(s);"))

(deftest-js-run js-e2e-for-of-string
  "for...of iterates characters of a string."
  ("h,e,l,l,o" "const chars=[]; for(let c of 'hello'){chars.push(c);} console.log(chars.join(','));"))

(deftest-js-run js-e2e-for-of-destructuring
  "for...of with array/object destructuring unpacks each element."
  ("1a,2b"   "const pairs=[[1,'a'],[2,'b']]; const out=[]; for(let [n,s] of pairs){out.push(n+s);} console.log(out.join(','));")
  ("Alice,Bob" "const people=[{name:'Alice'},{name:'Bob'}]; const names=[]; for(const {name} of people){names.push(name);} console.log(names.join(','));")
  ("1,2,3"   "const rows=[[1,2,3]]; for(const [a,...rest] of rows){console.log([a].concat(rest).join(','));}"))

;;; ─── Generators + Map/Set + Array.from ──────────────────────────────────────

(deftest-js-run js-e2e-generator-return-and-throw
  "Explicit .return()/.throw() calls on a suspended generator -- previously
untested anywhere in this suite, despite %js-make-generator's coroutine
channel (src/runtime-generator.lisp) being rewritten onto cl-concurrent-kit
this session."
  ("42"        "function* g(){yield 1; yield 2;} const it=g(); it.next(); console.log(it.return(42).value);")
  ("cleanup"   "function* g(){try{yield 1; yield 2;}finally{console.log('cleanup');}} const it=g(); it.next(); it.return(42);")
  (#.(format nil "caught:boom~%2") "function* g(){try{yield 1;}catch(e){console.log('caught:'+e); yield 2;}} const it=g(); it.next(); console.log(it.throw('boom').value);"))

(deftest-js-run js-e2e-generator-execution
  "Generator functions eagerly collect yields; for...of and spread drain them."
  ("0,1,2,3,4" "function* range(n){for(let i=0;i<n;i++){yield i;}} const out=[]; for(const v of range(5)){out.push(v);} console.log(out.join(','));")
  ("1,2,3"     "function* g(){yield 1; yield 2; yield 3;} console.log([...g()].join(','));")
  ("a,b,c"     "function* letters(){yield* ['a','b','c'];} const out=[]; for(const v of letters()){out.push(v);} console.log(out.join(','));"))

(deftest-js-run js-e2e-map-iteration
  "Map for...of yields [k,v] pairs; .size/.get/.set/.has work; Array.from(map) works."
  ("a=1,b=2"       "const m=new Map([['a',1],['b',2]]); const out=[]; for(const [k,v] of m){out.push(k+'='+v);} console.log(out.join(','));")
  ("2 1 true false" "const m=new Map(); m.set('x',1); m.set('y',2); console.log(m.size+' '+m.get('x')+' '+m.has('x')+' '+m.has('z'));")
  ("2"              "const m=new Map([[1,'a'],[2,'b']]); console.log(Array.from(m).length);"))

(deftest-js-run js-e2e-set-iteration
  "Set for...of yields unique values; .size/.has/.add/.delete work; spread works."
  ("1,2,3"     "const s=new Set([1,2,3,2,1]); const out=[]; for(const v of s){out.push(v);} console.log(out.join(','));")
  ("3 true false" "const s=new Set([1,2,3]); console.log(s.size+' '+s.has(2)+' '+s.has(9));")
  ("4"          "const a=new Set([1,2,3]); const b=new Set([3,4]); const u=new Set([...a,...b]); console.log(u.size);"))

(deftest-js-run js-e2e-array-from
  "Array.from converts iterables/array-likes; optional map function applied."
  ("1,2,3"   "console.log(Array.from([1,2,3]).join(','));")
  ("h,e,l,l,o" "console.log(Array.from('hello').join(','));")
  ("2,4,6"   "console.log(Array.from([1,2,3],x=>x*2).join(','));")
  ("3"       "console.log(Array.from(new Set([1,2,3])).length);")
  ;; Array-LIKE (non-iterable: no Symbol.iterator/next) object -- the
  ;; {length, 0, 1, ...} path, distinct code from the iterable path above
  ;; and previously never exercised by any test.
  ("a,b,c"   "console.log(Array.from({length:3,0:'a',1:'b',2:'c'}).join(','));"))

;;; ─── Math static methods ──────────────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-math-static-methods
  "Math.* trig/rounding/misc static methods compute JS-faithful values."
  ("3"       "console.log(Math.floor(3.7));")
  ("4"       "console.log(Math.ceil(3.2));")
  ("4"       "console.log(Math.round(3.5));")
  ("-3"      "console.log(Math.trunc(-3.7));")
  ("-1"      "console.log(Math.sign(-5));")
  ("1"       "console.log(Math.min(3,1,2));")
  ("1024"    "console.log(Math.pow(2,10));")
  ("1"       "console.log(Math.log(Math.E));")
  ("0"       "console.log(Math.sin(0));")
  ("1"       "console.log(Math.cos(0));")
  ("0"       "console.log(Math.tan(0));")
  ("1.5707963267948966" "console.log(Math.asin(1));")
  ("0"       "console.log(Math.acos(1));")
  ("0"       "console.log(Math.atan(0));")
  ("0"       "console.log(Math.sinh(0));")
  ("1"       "console.log(Math.cosh(0));")
  ("0"       "console.log(Math.tanh(0));")
  ("0"       "console.log(Math.asinh(0));")
  ("0"       "console.log(Math.acosh(1));")
  ("0"       "console.log(Math.atanh(0));")
  ("3"       "console.log(Math.cbrt(27));")
  ("1"       "console.log(Math.exp(0));")
  ("5"       "console.log(Math.hypot(3,4));")
  ("1.5"     "console.log(Math.fround(1.5));")
  ("12"      "console.log(Math.imul(3,4));")
  ("number"  "console.log(typeof Math.random());"))

;;; ─── Number static methods ────────────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-number-static-methods
  "Number.isFinite/isNaN/parseFloat/parseInt and EPSILON/NaN statics."
  ("true"    "console.log(Number.isFinite(1));")
  ("false"   "console.log(Number.isFinite(Infinity));")
  ("true"    "console.log(Number.isNaN(NaN));")
  ("false"   "console.log(Number.isNaN(1));")
  ;; Number.parseInt/parseFloat are, per spec, === the global parseInt/
  ;; parseFloat -- they parse a leading numeric PREFIX and ignore trailing
  ;; junk, unlike ToNumber coercion (which requires the whole string to be
  ;; numeric). The "NaN" expectations here were WRONG, locking in a real
  ;; bug (Number.parseInt/parseFloat were wired to %js-to-number instead
  ;; of %js-parse-int/%js-parse-float) -- see the release notes.
  ("3.14"    "console.log(Number.parseFloat('3.14x'));")
  ("42"      "console.log(Number.parseInt('42x'));")
  ("true"    "console.log(Number.EPSILON > 0);")
  ("false"   "console.log(Number.isNaN(Number.NaN));"))

;;; ─── Error class hierarchy: EvalError/URIError message wiring ────────────────
;;;
;;; Regression: EvalError/URIError classes existed in *js-builtin-specs* but
;;; were never added to *js-prelude-global-specs*, so the bare identifiers
;;; resolved to undefined and `new EvalError(...)` silently built an empty
;;; object instead of routing through the real error constructor.

(deftest-js-run-isolated-batch js-e2e-error-class-hierarchy-extra
  "TypeError/ReferenceError/SyntaxError/EvalError/URIError carry their message."
  ("object"  "console.log(typeof new TypeError('x'));")
  ("x"       "console.log(new TypeError('x').message);")
  ("y"       "console.log(new ReferenceError('y').message);")
  ("z"       "console.log(new SyntaxError('z').message);")
  ("e"       "console.log(new EvalError('e').message);")
  ("u"       "console.log(new URIError('u').message);"))

;;; ─── String static methods ────────────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-string-static-methods
  "String.fromCharCode/fromCodePoint build strings from numeric code points.
String.raw returns the RAW (as-written, escapes untouched) template text --
fixed 2026-07-31, see the release notes; previously returned cooked output
because the template lexer only tracked the escapes-processed string."
  ("AB"      "console.log(String.fromCharCode(65,66));")
  ("1"       "console.log(String.fromCodePoint(0x1F600).length);")
  ("a\\nb"   "console.log(String.raw`a\\nb`);")
  ("1\\t2"   "console.log(String.raw`${1}\\t${2}`);"))

;;; ─── Array.of / Array.prototype.toLocaleString ───────────────────────────────
;;;
;;; Regression: Array.prototype.toLocaleString had no method-table entry at
;;; all, so calling it crashed with an internal \"Undefined function\" error
;;; instead of a normal JS result.

(deftest-js-run-isolated-batch js-e2e-array-of-and-locale-string
  "Array.of builds an array from its arguments; toLocaleString joins like join(',')."
  ("1,2,3"   "console.log(Array.of(1,2,3).join(','));")
  ("1,2"     "console.log([1,2].toLocaleString());"))

;;; ─── Arbitrary named properties on arrays ──────────────────────────────────
;;;
;;; Regression: `arr.foo = 1` on a JS array silently no-op'd (arrays are CL
;;; vectors here, which have no room for named properties) — found and fixed
;;; 2026-07-31 while building real .raw support for tagged templates
;;; (String.raw needed the exact same "array with an extra named property"
;;; capability), see *js-array-extra-properties* (runtime-property.lisp).

(deftest-js-run-isolated-batch js-e2e-array-extra-properties
  "A JS array is a real object underneath its exotic index behavior — it can
carry ordinary named properties alongside its elements, same as any object."
  ("1"       "let a=[1,2]; a.foo=1; console.log(a.foo);")
  ("true"    "let a=[1,2]; a.foo=1; console.log('foo' in a);")
  ("false"   "let a=[1,2]; console.log('foo' in a);")
  ("2"       "let a=[1,2]; a.foo=1; console.log(a.length);")
  ("1,2"     "let a=[1,2]; a.foo=1; console.log(a.join(','));"))

;;; ─── Old-style function constructors ──────────────────────────────────────────
;;;
;;; Regression: %js-new had no branch that bound `this` to a fresh instance
;;; for a bare function/vm-closure constructor at all — `new F(x)` on an
;;; old-style `function F(x){this.val=x;}` silently produced an empty object,
;;; discarding every `this.prop = ...` assignment in the constructor body.
;;; ES6 class constructors were unaffected (separate class-lowering path).

(deftest-js-run-isolated-batch js-e2e-old-style-function-constructors
  "new F(...) on a plain `function F(){this.x=...}` binds `this` to the new instance."
  ("5"          "function F(x){this.val=x;} console.log(new F(5).val);")
  ("{\"val\":5}" "function F(x){this.val=x;} console.log(JSON.stringify(new F(5)));")
  ("1,2"        "function Point(x,y){this.x=x;this.y=y;} const p=new Point(1,2); console.log(p.x+','+p.y);")
  ("5"          "function F(x){this.val=x;} console.log(Reflect.construct(F,[5]).val);"))

;;; ─── Object static reflection methods ─────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-object-static-reflection
  "Object.freeze/isFrozen/seal/isSealed/preventExtensions/isExtensible and
property-descriptor introspection."
  ("1"       "console.log(Object.freeze({a:1}).a);")
  ("true"    "console.log(Object.isFrozen(Object.freeze({})));")
  ("true"    "console.log(Object.isSealed(Object.seal({})));")
  ("true"    "console.log(Object.isExtensible({}));")
  ("false"   "console.log(Object.isExtensible(Object.preventExtensions({})));")
  ("a,b"     "console.log(Object.getOwnPropertyNames({a:1,b:2}).join(','));")
  ("1"       "console.log(Object.getOwnPropertyDescriptor({a:1},'a').value);"))

;;; ─── Reflect static methods ────────────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-reflect-static-methods
  "Reflect.get/has/set/deleteProperty/apply/isExtensible mirror the low-level
[[...]] object operations."
  ("1"       "console.log(Reflect.get({a:1},'a'));")
  ("true"    "console.log(Reflect.has({a:1},'a'));")
  ("true"    "console.log(Reflect.set({}, 'a', 5));")
  ("true"    "console.log(Reflect.deleteProperty({a:1},'a'));")
  ("3"       "console.log(Reflect.apply(function(a,b){return a+b;},null,[1,2]));")
  ("true"    "console.log(Reflect.isExtensible({}));"))

;;; ─── BigInt static methods ─────────────────────────────────────────────────────
;;;
;;; Regression: the prelude bound BigInt as a bare :function (just the
;;; coercion callable), so BigInt.asIntN/asUintN had no object to live on and
;;; crashed with an internal \"Undefined function\" error. BigInt is now a
;;; constructor object like Date, with a __call__ for BigInt(x) coercion.

(deftest-js-run-isolated-batch js-e2e-bigint-static-methods
  "BigInt(x) coerces; BigInt.asIntN/asUintN mask to signed/unsigned N-bit ints."
  ("5"       "console.log(BigInt(5));")
  ("-1"      "console.log(BigInt.asIntN(8, 255n));")
  ("255"     "console.log(BigInt.asUintN(8, -1n));"))
