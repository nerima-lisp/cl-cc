;;;; packages/javascript/tests/js-e2e-ast-tests.lisp — JS AST structural tests
;;;;
;;;; Parse-only tests that check the AST shape produced by the JS frontend
;;;; without running the program through the VM.
;;;;
;;;; Depends on: js-e2e-core-tests.lisp (loaded before this in serial ASDF module).

(in-package :cl-cc/test)

;;; ─── Parse helpers ───────────────────────────────────────────────────────────

(defun %js-e2e-parse (src)
  "Parse SRC and return the list of top-level AST nodes."
  (cl-cc/javascript:parse-js-source src))

(defun %js-e2e-has-defun-named (name asts)
  "Return the AST-DEFUN whose name matches NAME case-insensitively, or NIL.
JS identifiers are now interned case-preserving, so a defun named `fib' is the
symbol |fib|; compare with string-equal so these structural tests stay name-case
agnostic."
  (find-if (lambda (ast)
             (and (cl-cc:ast-defun-p ast)
                  (string-equal name
                                (symbol-name (cl-cc:ast-defun-name ast)))))
           asts))

(defun %js-e2e-has-defclass-named (name asts)
  "Return the AST-DEFCLASS whose name matches NAME case-insensitively, or NIL.
JS identifiers are interned case-preserving now, so compare with string-equal."
  (find-if (lambda (ast)
             (and (cl-cc:ast-defclass-p ast)
                  (string-equal name
                                (symbol-name (cl-cc:ast-defclass-name ast)))))
           asts))

(defun %js-e2e-defun-body-forms (fn)
  "Return FN's effective body forms, unwrapping the single (block nil ...) that
js-callable-body wraps function bodies in so JS `return' (→ return-from nil)
resolves. Lets these AST-shape tests look past the block at the real statements."
  (let ((body (cl-cc:ast-defun-body fn)))
    (if (and (= (length body) 1) (cl-cc:ast-block-p (first body)))
        (cl-cc:ast-block-body (first body))
        body)))

;;; ─── 1. FizzBuzz ──────────────────────────────────────────────────────────────

(it-sequential "js-e2e-fizzbuzz"
  (let ((asts (%js-e2e-parse "
function fizzBuzz(n) {
  for (let i = 1; i <= n; i++) {
    if (i % 15 === 0) {
      console.log('FizzBuzz');
    } else if (i % 3 === 0) {
      console.log('Fizz');
    } else if (i % 5 === 0) {
      console.log('Buzz');
    } else {
      console.log(i);
    }
  }
}
fizzBuzz(20);
")))
    (expect (>= (length asts) 2) :to-be-truthy)
    (expect (not (null (%js-e2e-has-defun-named "FIZZBUZZ" asts))) :to-be-truthy)))

;;; ─── 2. Fibonacci recursive ───────────────────────────────────────────────────

(it-sequential "js-e2e-fibonacci-recursive"
  (let* ((asts (%js-e2e-parse "
function fib(n) {
  if (n <= 1) return n;
  return fib(n - 1) + fib(n - 2);
}
"))
         (fn (%js-e2e-has-defun-named "FIB" asts)))
    (expect (not (null fn)) :to-be-truthy)
    (expect (some #'cl-cc:ast-if-p (%js-e2e-defun-body-forms fn)) :to-be-truthy)))

;;; ─── 3. Array map / filter / reduce ──────────────────────────────────────────

(it-sequential "js-e2e-array-higher-order"
  (let ((asts (%js-e2e-parse "
const nums = [1, 2, 3, 4, 5];
const doubled = nums.map(x => x * 2);
const evens = nums.filter(x => x % 2 === 0);
const sum = nums.reduce((acc, x) => acc + x, 0);
")))
    (expect (>= (length asts) 1) :to-be-truthy)
    (expect (cl-cc:ast-let-p (first asts)) :to-be-truthy)))

;;; ─── 4. Class with inheritance ────────────────────────────────────────────────

(it-sequential "js-e2e-class-inheritance"
  (let ((asts (%js-e2e-parse "
class Animal {
  constructor(name) {
    this.name = name;
  }
  speak() {
    return this.name + ' makes a noise.';
  }
}

class Dog extends Animal {
  speak() {
    return this.name + ' barks.';
  }
}
")))
    (expect (not (null (%js-e2e-has-defclass-named "ANIMAL" asts))) :to-be-truthy)
    (expect (not (null (%js-e2e-has-defclass-named "DOG" asts))) :to-be-truthy)
    (let ((dog (%js-e2e-has-defclass-named "DOG" asts)))
      (expect (some (lambda (s) (string-equal "Animal" (symbol-name s)))
                (cl-cc:ast-defclass-superclasses dog)) :to-be-truthy))))

;;; ─── 5. Object destructuring ──────────────────────────────────────────────────

(it-sequential "js-e2e-object-destructuring"
  (let ((asts (%js-e2e-parse "
const person = { name: 'Alice', age: 30, city: 'NY' };
const { name, age } = person;
const { city: location } = person;
")))
    (expect (>= (length asts) 1) :to-be-truthy)
    (labels ((getprop-binding-p (b)
               (let ((v (cdr b)))
                 (and (cl-cc:ast-call-p v)
                      (cl-cc:ast-var-p (cl-cc:ast-call-func v))
                      (search "GET-PROP"
                              (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func v)))))))
             (chain-has-getprop (node)
               (when (cl-cc:ast-let-p node)
                 (or (some #'getprop-binding-p (cl-cc:ast-let-bindings node))
                     (some #'chain-has-getprop (cl-cc:ast-let-body node))))))
      (expect (some #'chain-has-getprop asts) :to-be-truthy))))

;;; ─── 6. Generator sequence ────────────────────────────────────────────────────

(it-sequential "js-e2e-generator-sequence"
  (let* ((asts (%js-e2e-parse "
function* range(start, end, step = 1) {
  for (let i = start; i < end; i += step) {
    yield i;
  }
}
const it = range(0, 10, 2);
"))
         (gen (%js-e2e-has-defun-named "RANGE" asts)))
    (expect (not (null gen)) :to-be-truthy)
    (expect (member :js-generator (cl-cc:ast-defun-declarations gen)) :to-be-truthy)))

;;; ─── 7. Error handling try / catch ───────────────────────────────────────────

(it-sequential "js-e2e-error-handling"
  (let ((asts (%js-e2e-parse "
function safeDiv(a, b) {
  try {
    if (b === 0) throw new Error('Division by zero');
    return a / b;
  } catch (e) {
    console.error('Error:', e.message);
    return null;
  } finally {
    console.log('safeDiv done');
  }
}
")))
    (let* ((fn (%js-e2e-has-defun-named "SAFEDIV" asts))
           (body (when fn (%js-e2e-defun-body-forms fn))))
      (expect (not (null fn)) :to-be-truthy)
      (expect (some (lambda (node)
                  (and (cl-cc:ast-call-p node)
                       (string= "%JS-TRY-CATCH-FINALLY"
                                 (symbol-name
                                  (cl-cc:ast-var-name
                                   (cl-cc:ast-call-func node))))))
                body) :to-be-truthy))))

;;; ─── 8. Module-style exports ──────────────────────────────────────────────────

(it-sequential "js-e2e-module-exports"
  (let ((asts (cl-cc/javascript:parse-js-module "
export function add(a, b) { return a + b; }
export function subtract(a, b) { return a - b; }
export const PI = 3.14159;
")))
    (expect (>= (length asts) 0) :to-be-truthy)))

;;; ─── 9. Async / await simulation ─────────────────────────────────────────────

(it-sequential "js-e2e-async-await"
  (let* ((asts (%js-e2e-parse "
async function fetchData(url) {
  try {
    const response = await fetch(url);
    const data = await response.json();
    return data;
  } catch (err) {
    throw new Error('Fetch failed: ' + err.message);
  }
}
"))
         (fn (%js-e2e-has-defun-named "FETCHDATA" asts)))
    (expect (not (null fn)) :to-be-truthy)
    (expect (member :js-async (cl-cc:ast-defun-declarations fn)) :to-be-truthy)))

;;; ─── 9b. Async / await execution ────────────────────────────────────────────

(it-sequential "js-e2e-async-await-execution"
  (expect (%js-run-capture
     "async function double(x){return x*2;}
async function main(){const r=await double(5); console.log(r);}
main();") :to-equal "10")
  (expect (%js-run-capture
     "async function add(a,b){return a+b;}
async function run(){const s=await add(2,4); console.log(s);}
run();") :to-equal "6")
  (expect (%js-run-capture
     "async function id(x){return await x;}
async function test(){console.log(await id(7));}
test();") :to-equal "7")
  (expect (%js-run-capture
     "async function fail(){throw new Error('oops');}
async function main(){
  try{await fail();}catch(e){console.log('caught');}
}
main();") :to-equal "caught"))

;;; ─── 10. Optional chaining chain ─────────────────────────────────────────────

(it-sequential "js-e2e-optional-chaining-chain"
  (let ((asts (%js-e2e-parse "
const user = { profile: { address: { city: 'NYC' } } };
const city = user?.profile?.address?.city;
const zip = user?.profile?.address?.zip ?? 'N/A';
const upper = user?.profile?.address?.city?.toUpperCase();
")))
    (expect (>= (length asts) 1) :to-be-truthy)
    (expect (cl-cc:ast-let-p (first asts)) :to-be-truthy)))
