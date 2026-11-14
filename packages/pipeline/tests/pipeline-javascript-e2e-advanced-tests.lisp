;;;; t/e2e-advanced-test.lisp — JS advanced execution tests, part 1
;;;;
;;;; Optional chaining, named function expressions, typeof, arrow params,
;;;; getters/setters, static namespaces, operators, coercion, tagged
;;;; templates, and standalone global builtins. Part 2 (class features
;;;; onward — split out once this file passed the org's 500-line cap) is
;;;; e2e-advanced-builtins-test.lisp.
;;;;
;;;; Depends on: e2e-core-test.lisp (%js-run-capture, deftest-js-run).

(in-package :cl-cc/test)

;;; ─── Optional chaining + nullish coalescing ──────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-optional-chaining-execution
  "?. returns undefined when chain is null/undefined; ?? returns left operand unless null/undefined."
  ("NYC"       "const u={profile:{city:'NYC'}}; console.log(u?.profile?.city);")
  ("undefined" "const u=null; console.log(u?.profile?.city);")
  ("undefined" "const u={profile:null}; console.log(u?.profile?.city);")
  ("N/A"       "const u={profile:{}}; console.log(u?.profile?.zip ?? 'N/A');")
  ("0"         "console.log(0 ?? 'fallback');")
  (""          "console.log('' ?? 'fallback');")
  ("false"     "console.log(false ?? 'fallback');")
  ("HELLO"     "const s='hello'; console.log(s?.toUpperCase());")
  ("undefined" "const s=null; console.log(s?.toUpperCase());")
  ("b"         "console.log(null ?? undefined ?? 'b' ?? 'c');"))

;;; ─── Named function expressions + typeof ─────────────────────────────────────

(deftest-js-run js-e2e-named-function-expression-recursion
  "Named function expressions can self-recurse by name; name visible only inside body."
  ("120"   "const f=function fac(n){return n<=1?1:n*fac(n-1);}; console.log(f(5));")
  ("55"    "const f=function fib(n){return n<2?n:fib(n-1)+fib(n-2);}; console.log(f(10));")
  ("5"     "const f=function(x){return x+1;}; console.log(f(4));")
  ("6"     "const f=function g(x){return x*2;}; console.log(f(3));")
  ("6,2,4" "console.log([3,1,2].map(function dbl(x){return x*2;}).join(\",\"));"))

(deftest-js-run-isolated-batch js-e2e-typeof-function
  "typeof returns 'function' for vm-closure-objects; non-function types unchanged."
  ("function"           "const f=function(x){return x;}; console.log(typeof f);")
  ("function"           "const g=x=>x; console.log(typeof g);")
  ("function"           "function h(){return 1;} console.log(typeof h);")
  ("function"           "const o={m(){return 1;}}; console.log(typeof o.m);")
  ("yes"                "const h=function(){}; console.log(typeof h===\"function\"?\"yes\":\"no\");")
  ("object"             "console.log(typeof {a:1});")
  ("object"             "console.log(typeof [1,2]);")
  ("string number boolean" "console.log(typeof \"x\", typeof 5, typeof true);"))

;;; ─── Arrow function params ───────────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-arrow-default-and-rest-params
  "Arrow functions support default parameters and ...rest collection."
  ("10" "const f=(x=5)=>x*2; console.log(f());")
  ("6"  "const f=(x=5)=>x*2; console.log(f(3));")
  ("12" "const f=(a,b=2)=>a+b; console.log(f(10));")
  ("15" "const f=(a,b=2)=>a+b; console.log(f(10,5));")
  ("3"  "const f=(...n)=>n.length; console.log(f(1,2,3));")
  ("10" "const f=(...n)=>n.reduce((a,b)=>a+b,0); console.log(f(1,2,3,4));")
  ("12" "const f=(a,...n)=>a+n.length; console.log(f(10,1,2));")
  ("6"  "const f=(x=5)=>{return x+1;}; console.log(f());")
  ("7"  "const f=(a,b)=>a+b; console.log(f(3,4));")
  ("7"  "const add=a=>b=>a+b; console.log(add(3)(4));"))

;;; ─── Object-literal getters and setters ──────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-object-getters-setters
  "Object-literal get/set accessors invoked on property read/write; both survive on same key."
  ("42" "const o={get v(){return 42;}}; console.log(o.v);")
  ("20" "const o={n:10,get v(){return this.n*2;}}; console.log(o.v);")
  ("5"  "const o={_x:0,set v(n){this._x=n;}}; o.v=5; console.log(o._x);")
  ("11" "const o={_c:0,get count(){return this._c;},set count(n){this._c=n+1;}}; o.count=10; console.log(o.count);")
  ("5"  "const o={n:5,m(){return this.n;}}; console.log(o.m());")
  ("3"  "const o={a:1,b:2}; console.log(o.a+o.b);"))

;;; ─── Static-method namespace globals ─────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-static-method-namespaces
  "Object/Reflect/Number/Array/String namespace globals seeded in prelude; Math/JSON unaffected."
  ("a,b"            "console.log(Object.keys({a:1,b:2}).join(\",\"));")
  ("2"              "console.log(Object.keys({a:1,b:2}).length);")
  ("1,2"            "console.log(Object.values({a:1,b:2}).join(\",\"));")
  ("1"              "console.log(Object.entries({a:1}).length);")
  ("2"              "console.log(Object.assign({},{a:1},{b:2}).b);")
  ("true"           "console.log(Array.isArray([1,2]));")
  ("false"          "console.log(Array.isArray(5));")
  ("true"           "console.log(Number.isInteger(5));")
  ("2"              "console.log(Reflect.ownKeys({a:1,b:2}).length);")
  ("9007199254740991" "console.log(Number.MAX_SAFE_INTEGER);")
  ("2"              "console.log(Math.max(1,2));")
  ("{\"a\":1}"      "console.log(JSON.stringify({a:1}));"))

;;; ─── Relational operators ────────────────────────────────────────────────────

(deftest-js-run js-e2e-relational-operators
  "Relational and comparison operators, including string/NaN coercion edge cases."
  ("true"  "console.log(5 > 0);")
  ("false" "console.log(2 < 1);")
  ("true"  "console.log(3 >= 3);")
  ("false" "console.log(4 <= 2);")
  ("true"  "console.log(\"apple\" < \"banana\");")
  ("false" "console.log(NaN > 0);")
  ("true"  "console.log(\"5\" > 3);")
  ("y"     "console.log(5 > 3 ? \"y\" : \"n\");")
  ("6"     "let s=0; for(let i=0;i<4;i++){s+=i;} console.log(s);"))

;;; ─── Logical &&/|| ──────────────────────────────────────────────────────────

(deftest-js-run js-e2e-logical-and-or
  "&&/|| short-circuit evaluation and value-returning (not boolean-coercing) semantics."
  ("false" "console.log(true && false);")
  ("true"  "console.log(true && true);")
  ("2"     "console.log(1 && 2);")
  ("0"     "console.log(0 && 2);")
  ("5"     "console.log(0 || 5);")
  ("3"     "console.log(3 || 5);")
  ("b"     "console.log(\"a\" && \"b\");")
  ("3"     "console.log(1 && 2 && 3);")
  ("def"   "function f(a){return a || \"def\";} console.log(f());")
  ("hi"    "function f(a){return a || \"def\";} console.log(f(\"hi\"));")
  ("0"     "let c=0; function s(){c++;return true;} false && s(); console.log(c);"))

;;; ─── null / undefined literals ───────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-null-undefined-literals
  "null/undefined lower to runtime sentinels; ?? treats them as nullish; typeof is correct."
  ("null"      "console.log(null);")
  ("undefined" "console.log(undefined);")
  ("null"      "const x=null; console.log(x);")
  ("d"         "console.log(null ?? \"d\");")
  ("d"         "console.log(undefined ?? \"d\");")
  ("null"      "const x=null; console.log(x && x.foo);")
  ("object"    "console.log(typeof null);")
  ("undefined" "console.log(typeof undefined);")
  ("true"      "console.log(null == undefined);")
  ("v=null"    "console.log(\"v=\"+null);")
  ("undefined" "console.log(void 0);"))

;;; ─── Class getters and setters ───────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-class-getters-setters
  "Class get/set accessors are invoked on property read/write; inheritance resolves through prototype chain."
  ("9"  "class C{get v(){return 9;}} console.log(new C().v);")
  ("20" "class C{constructor(){this.n=10;} get v(){return this.n*2;}} console.log(new C().v);")
  ("5"  "class C{set v(x){this._x=x;}} const o=new C(); o.v=5; console.log(o._x);")
  ("8"  "class C{constructor(){this._x=1;} get x(){return this._x;} set x(v){this._x=v;}} const o=new C(); o.x=8; console.log(o.x);")
  ("1"  "class A{get v(){return 1;}} class B extends A{} console.log(new B().v);")
  ("3"  "class C{m(){return 3;}} console.log(new C().m());")
  ("7"  "class C{constructor(){this.x=7;}} console.log(new C().x);")
  ;; A method's default-parameter value that is itself a call
  ;; (%js-parse-method-params-body's default-value skip loop has no nesting
  ;; tracking, mirroring the decorator-argument bug this session found and
  ;; fixed) must not corrupt parsing of whatever follows it in the class
  ;; body -- m2 here is the check, not m1's own (currently unapplied, a
  ;; separate known gap) default value.
  ("99" "class C{m1(a,b=foo(1,2)){return a;} m2(){return 99;}} console.log(new C().m2());"))

;;; ─── Computed class member names ─────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-class-computed-member-names
  "A computed member name ([expr]) in a class body is parsed as a real runtime
key expression -- not discarded into a meaningless gensym placeholder -- so
obj.name / obj[key] / Class.name all resolve computed instance methods,
instance fields, static methods, and get/set accessors under the SAME key
the bracketed expression evaluates to."
  ("1"    "class C{['m'+'ethod'](){return 1;}} console.log(new C().method());")
  ("hi"   "const k='greet'; class C{[k](){return 'hi';}} console.log(new C().greet());")
  ("5"    "class C{['x']=5;} console.log(new C().x);")
  ("9"    "class C{static ['make'+'r'](){return 9;}} console.log(C.maker());")
  ("42"   "class C{get ['va'+'l'](){return 42;}} console.log(new C().val);")
  ("7"    "class C{set ['va'+'l'](v){this._v=v*7;}} const c=new C(); c.val=1; console.log(c._v);")
  ("true" "class C{[Symbol.iterator](){return 1;}} const c=new C(); console.log(c[Symbol.iterator]!==undefined);")
  ("1"    "class C{[Symbol.iterator](){return 1;}} const c=new C(); console.log(c[Symbol.iterator]());"))

;;; ─── Static initialisation blocks (ES2022) ───────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-class-static-blocks
  "`static { ... }` (%js-parse-class-static-block, src/parser-class.lisp) reuses
the ordinary static-field slot machinery (:allocation :class) so its body
runs once at class-definition time as a side effect of that slot's initform
being evaluated; the slot's own value (an unreferenced gensym name) is
never read. Genuinely untested anywhere in this suite before now despite
being explicitly listed in this file's own header comment as a supported
feature -- it turned out to not run AT ALL (the slot was never threaded
into the class's %js-make-class call; see the src/parser-class-lower.lisp
fix). These cases observe the block's side effect through an outer variable
rather than the class's own name -- referencing the class by its own name
from a static field/block initializer (as opposed to from inside a method
body, which is lazily invoked later once the binding exists) is a separate,
deeper, not-yet-fixed limitation; see docs/src/reference/compatibility.md."
  ("42" "let se=0; class C{static {se=42;}} console.log(se);")
  ("3"  "let se=0; class C{static {se+=1; se+=2;}} console.log(se);")
  ("ab" "let se=''; class C{static {se+='a';} static {se+='b';}} console.log(se);"))

;;; ─── Private fields and methods (#name) ──────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-private-fields-and-methods
  "Private fields/methods (#name) are listed as a supported feature in this
file's own header comment, and `js-parser-class-field-private-arrow-and-
method' (t/parser-decl-test.lisp) checks they parse into class slots, but
no test anywhere actually CALLS a private method or reads/writes a private
field through real execution -- genuinely untested end to end before now."
  ("5"    "class C{#x=5; getX(){return this.#x;}} console.log(new C().getX());")
  ("42"   "class C{#secret(){return 42;} reveal(){return this.#secret();}} console.log(new C().reveal());")
  ("1 2"  "class C{#x=0; inc(){this.#x++; return this.#x;}} const c=new C(); console.log(c.inc()+' '+c.inc());")
  ("10"   "class C{#x; constructor(v){this.#x=v;} getX(){return this.#x;}} console.log(new C(10).getX());")
  ("true" "class C{#secret(){return 1;}} console.log(typeof new C().secret===\"undefined\");")
  ;; Static private methods (%js-class-static-slots,
  ;; src/parser-class-lower-classify.lisp)
  ;; have the same missing :js-private filter as the instance-method bug fixed
  ;; above -- checking whether the identical leak exists there too.
  ("1"    "class C{static #secret(){return 1;} static reveal(){return C.#secret();}} console.log(C.reveal());")
  ("true" "class C{static #secret(){return 1;}} console.log(typeof C.secret===\"undefined\");"))

;;; ─── Conditional truthiness ──────────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-conditional-truthiness
  "JS falsy values test as false in ternary/if; uninitialized `let' is undefined."
  ("f" "console.log(\"\" ? \"t\" : \"f\");")
  ("f" "console.log(null ? \"t\" : \"f\");")
  ("f" "console.log(undefined ? \"t\" : \"f\");")
  ("f" "console.log(NaN ? \"t\" : \"f\");")
  ("f" "console.log(0 ? \"t\" : \"f\");")
  ("t" "console.log(\"x\" ? \"t\" : \"f\");")
  ("t" "console.log([] ? \"t\" : \"f\");")
  ("f" "if(NaN){console.log(\"t\");}else{console.log(\"f\");}")
  ("undefined" "let x; console.log(typeof x);")
  ("true"      "let y; console.log(y === undefined);")
  ("f"         "let z; console.log(z ? \"t\" : \"f\");"))

;;; ─── Coercion functions ──────────────────────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-coercion-functions
  "Number/String/Boolean/parseInt/parseFloat callable as functions; Number.isInteger unaffected."
  ("43"         "console.log(Number(\"42\") + 1);")
  ("3.14"       "console.log(Number(\"3.14\"));")
  ("0"          "console.log(Number());")
  ("42x"        "console.log(String(42) + \"x\");")
  ("null"       "console.log(String(null));")
  ("true false" "console.log(Boolean(1), Boolean(0));")
  ("false true" "console.log(Boolean(\"\"), Boolean(\"x\"));")
  ("42"         "console.log(parseInt(\"42px\"));")
  ("255"        "console.log(parseInt(\"ff\", 16));")
  ;; "0x"/"0X" hex-prefix auto-detection when radix is omitted (or 0) --
  ;; previously entirely missing (parseInt("0x1F") returned 0, only the
  ;; leading "0" parsed in the wrongly-assumed base-10 default). Explicit
  ;; radix 10 must NOT auto-detect the prefix (stops at "0"), per spec.
  ("31"         "console.log(parseInt(\"0x1F\"));")
  ("31"         "console.log(parseInt(\"0X1f\"));")
  ("-16"        "console.log(parseInt(\"-0x10\"));")
  ("0"          "console.log(parseInt(\"0x1F\", 10));")
  ("3.14"       "console.log(parseFloat(\"3.14abc\"));")
  ("1500"       "console.log(parseFloat(\"1.5e3xyz\"));")
  ("NaN"        "console.log(parseFloat(\"abc\"));")
  ("true"       "console.log(Number.isInteger(5));"))

;;; ─── Tagged templates + Number.prototype ─────────────────────────────────────

(deftest-js-run-isolated-batch js-e2e-tagged-templates
  "Tagged template calls TAG(strings-array, ...subs); plain templates concatenate.
The strings-array's `raw` property (last two cases) is checked through a
custom tag function, not just the built-in String.raw (js-e2e-string-static-
methods) -- proving the general TC39 tagged-template protocol works for any
tag, not one hardcoded to the built-in."
  ("1"     "function t(s){return s.length;} console.log(t`hi`);")
  ("hello" "function t(s){return s[0];} console.log(t`hello`);")
  ("a5b"   "function t(s,v){return s[0]+v+s[1];} console.log(t`a${5}b`);")
  ("x1y2z" "function t(s,a,b){return s[0]+a+s[1]+b+s[2];} console.log(t`x${1}y${2}z`);")
  ("a|b|c" "function tag(s,...v){return s.join('|');} console.log(tag`a${1}b${2}c`);")
  ("[]9[]" "function t(s,v){return '['+s[0]+']'+v+'['+s[1]+']';} console.log(t`${9}`);")
  ("val=5" "const n=5; console.log(`val=${n}`);")
  ("sum=5" "console.log(`sum=${2+3}`);")
  ("a\\nb" "function t(s){return s.raw[0];} console.log(t`a\\nb`);")
  ("2"     "function t(s){return s.raw.length;} console.log(t`x${1}\\ty`);"))

(deftest-js-run-isolated-batch js-e2e-number-prototype-methods
  "Number.prototype toFixed/toString/toPrecision/valueOf format JS-faithfully."
  ("123.46"   "console.log((123.456).toFixed(2));")
  ("8"        "console.log((7.5).toFixed(0));")
  ("ff"       "console.log((255).toString(16));")
  ("1010"     "console.log((10).toString(2));")
  ("FF"       "console.log((255).toString(16).toUpperCase());")
  ("3.14"     "console.log((3.14159).toPrecision(3));")
  ("100"      "console.log((100).toPrecision(3));")
  ("1.2e+3"   "console.log((1234.5).toPrecision(2));")
  ("0.000012" "console.log((0.00001234).toPrecision(2));")
  ("-3.14"    "console.log((-3.14159).toPrecision(3));")
  ("42"       "console.log((42).valueOf());")
  ("3.141590e+0" "console.log((3.14159).toExponential());")
  ("3.14e+0"  "console.log((3.14159).toExponential(2));")
  ("1234.5"   "console.log((1234.5).toLocaleString());"))

;;; ─── Standalone global builtins ──────────────────────────────────────────────

(deftest-js-run js-e2e-standalone-global-builtins
  "structuredClone/queueMicrotask are reachable as bare direct calls."
  ("3"       "console.log(structuredClone({b:[2,3]}).b[1]);")
  ("5,99"    "const o={x:{y:5}}; const c=structuredClone(o); c.x.y=99; console.log(o.x.y+','+c.x.y);")
  ("3"       "console.log(structuredClone([1,2,3]).length);")
  ("micro"   "queueMicrotask(()=>console.log('micro'));")
  ("1"       "const f=structuredClone; console.log(f({a:1}).a);")
  ("42"      "console.log(parseInt('42px'));"))
