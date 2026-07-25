# Core concepts

This page describes the Common Lisp language surface cl-cc implements. For
what is *not* implemented, and for the frontends' own language surfaces, see
[Compatibility](compatibility.md).

## Special forms

All of the core ANSI special forms are implemented.

| Form | Status |
| -------------------------------------------- | ------ |
| `if`, `progn`, `block`/`return-from`         | ✓      |
| `tagbody`/`go`                               | ✓      |
| `catch`/`throw`, `unwind-protect`            | ✓      |
| `let`, `let*`, `setq`, `setf`                | ✓      |
| `flet`, `labels` (mutual recursion)          | ✓      |
| `lambda`, `defun`, `defvar`, `defparameter`  | ✓      |
| `defmacro`, `macrolet`                       | ✓      |
| `quote`, `the`, `values`                     | ✓      |
| `multiple-value-bind`, `multiple-value-call` | ✓      |
| `eval-when`                                  | ✓      |

## Closures and higher-order functions

Closures capture lexical variables by reference, so a captured binding can be
mutated and the change is visible to every closure over it.

```lisp
(let ((count 0))
  (let ((inc (lambda () (setq count (+ count 1))))
        (get (lambda () count)))
    (funcall inc)
    (funcall inc)
    (funcall get)))
;; => 2
```

Functions returning functions work as expected, which is what makes the
CPS-transformed output of the compiler runnable by the compiler itself:

```lisp
(labels ((make-adder (n) (lambda (x) (+ x n))))
  (let ((add5 (make-adder 5)))
    (funcall add5 37)))
;; => 42
```

## CLOS

The object system is implemented in full, including multiple dispatch,
inheritance chains, and `call-next-method`.

```lisp
(defclass shape ()
  ((color :initarg :color :reader shape-color)))

(defclass circle (shape)
  ((radius :initarg :radius :reader circle-radius)))

(defclass rectangle (shape)
  ((width  :initarg :width  :reader rectangle-width)
   (height :initarg :height :reader rectangle-height)))

(defgeneric area (shape))

(defmethod area ((c circle))
  (* 3.14159 (circle-radius c) (circle-radius c)))

(defmethod area ((r rectangle))
  (* (rectangle-width r) (rectangle-height r)))

(let ((c (make-instance 'circle    :color :red  :radius 5))
      (r (make-instance 'rectangle :color :blue :width 4 :height 6)))
  (list (area c) (area r)))
;; => (78.53975 24)
```

Dispatch at each call site is served by an inline cache that adapts from
monomorphic to polymorphic to megamorphic as more types are observed. See
[Architecture](architecture.md).

## Macros

`defmacro` runs at compile time through cl-cc's own evaluator, not the host
SBCL. Quasiquote and `gensym` behave as ANSI specifies.

```lisp
(defmacro my-when (condition &body body)
  `(if ,condition (progn ,@body) nil))

(defmacro swap! (a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (setq ,a ,b)
       (setq ,b ,tmp))))
```

Because quasiquote is compiled correctly inside `defun` bodies, a compiled
program can generate and expand its own macros — this is the mechanism behind
[self-hosting](architecture.md#self-hosting).

## Conditions and error handling

```lisp
(handler-case
    (error "something went wrong: ~A" 42)
  (error (e)
    (format nil "caught: ~A" e)))

(ignore-errors
  (/ 1 0))
```

The runtime also signals `storage-condition` at 80%, 90%, and 95% heap
pressure, and guards against runaway recursion with
`*max-call-stack-depth*`.

## Structures

`defstruct` is not a separate mechanism: it expands to a `defclass` plus a
constructor and a predicate, so structures participate in CLOS dispatch.

```lisp
(defstruct point
  (x 0)
  (y 0))

(let ((p (make-point :x 3 :y 4)))
  (+ (point-x p) (point-y p)))
;; => 7
```

## Multiple values

```lisp
(multiple-value-bind (q r)
    (floor 17 5)
  (list q r))
;; => (3 2)
```

## Loop

The `loop` macro supports the iteration, accumulation, and termination
clauses:

```lisp
(loop for x in '(1 2 3 4 5)
      when (oddp x)
      collect (* x x))
;; => (1 9 25)

(loop for i from 1 to 10
      sum i)
;; => 55
```

## Standard library

**Lists** — `cons`, `car`, `cdr`, all 28 `c*r` forms, `list`, `append`,
`nconc`, `reverse`, `nreverse`, `length`, `nth`, `nthcdr`, `last`, `butlast`,
`copy-list`

**Sequences** — `mapcar`, `mapc`, `mapcan`, `every`, `some`, `notany`,
`notevery`, `find`, `find-if`, `position`, `count`, `remove`, `remove-if`,
`remove-duplicates`, `sort`, `stable-sort`, `reduce`, `merge`, `substitute`,
`delete`

**Strings** — `string=`, `string<`, `string>`, `string-upcase`,
`string-downcase`, `string-capitalize`, `nstring-upcase`, `nstring-downcase`,
`nstring-capitalize`, `subseq`, `concatenate`, `string-trim`,
`string-left-trim`, `string-right-trim`, `search`

**Characters** — `char-code`, `code-char`, `char-upcase`, `char-downcase`,
`digit-char-p`, `alpha-char-p`, `char-name`, `name-char`, and every
comparison predicate (`char=`, `char<`, `char>`, `char<=`, `char>=`,
`char-equal`, `char-not-equal`, `char-lessp`, `char-greaterp`,
`char-not-greaterp`, `char-not-lessp`)

**Hash tables** — `make-hash-table`, `gethash`, `(setf gethash)`, `remhash`,
`clrhash`, `maphash`, `hash-table-count`

**Arrays** — `make-array` (adjustable, fill-pointer, displaced), `aref`,
`(setf aref)`, `vector-push`, `vector-push-extend`, `vector-pop`,
`fill-pointer`, `adjust-array`, `array-has-fill-pointer-p`,
`adjustable-array-p`, `array-element-type`, `array-rank`, `array-dimensions`,
`array-dimension`, `array-total-size`, `array-in-bounds-p`,
`row-major-aref`, `array-displacement`, and bit array operations

**Numbers** — full arithmetic, `floor`/`ceiling`/`truncate`/`round`, `mod`,
`rem`, `abs`, `max`, `min`, `expt`, `sqrt`, `exp`, `log`, the trigonometric
functions, `ash`, `logand`, `logior`, `logxor`, `lognot`

**I/O** — `format`, `print`, `princ`, `prin1`, `read-line`, `read-char`,
`write-char`, `with-open-file`, `with-output-to-string`, `read-sequence`,
`write-sequence`, `peek-char`, `unread-char`, `listen`, `clear-input`,
`clear-output`, `finish-output`, `force-output`

**Pretty printer** — `pprint-logical-block`, `pprint-indent`,
`pprint-newline`, `pprint-tab`, `pprint-dispatch-table`, `*print-pretty*`,
`*print-level*`, `*print-length*`, `*print-circle*`, `*print-readably*`,
`*print-base*`, `*print-radix*`

**Streams** — `broadcast-stream`, `concatenated-stream`, `echo-stream`,
`synonym-stream`, `two-way-stream`, `string-input-stream`,
`string-output-stream`, and fundamental-stream (the Gray Streams protocol)

**Unicode** — BMP General Category predicates, case folding, UTF-8
encoding and decoding, NFC/NFD normalization (Latin-1), syntax class
determination

**Time** — `get-universal-time`, `get-internal-real-time`,
`get-internal-run-time`, `sleep`, `encode-universal-time`,
`decode-universal-time`, the `time` macro

**Random** — `random-state` (MT19937), `make-random-state`, `*random-state*`,
`random`

**Environment** — `lisp-implementation-type`, `lisp-implementation-version`,
`machine-type`, `machine-version`, `software-type`, `software-version`,
`room`, `apropos`, `apropos-list`

**Predicates** — `numberp`, `integerp`, `stringp`, `symbolp`, `consp`,
`null`, `listp`, `functionp`, `characterp`, `vectorp`, `hash-table-p`
