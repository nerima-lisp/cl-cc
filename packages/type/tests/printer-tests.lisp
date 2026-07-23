;;;; tests/unit/type/printer-tests.lisp — Type Printer Tests
;;;;
;;;; Tests for src/type/printer.lisp:
;;;; type-to-string for all type-node subtypes, unparse-type,
;;;; list-interleave, looks-like-type-specifier-p.
;;;; Coverage goal: every defmethod clause + every data table entry.

(in-package :cl-cc/test)


(defmacro assert-when-present (value form)
  `(when ,value ,form))

;;; ─── type-to-string: basic types ───────────────────────────────────────────

(it-sequential "printer-primitive-types int"
  (destructuring-bind (type-node expected) (list type-int "FIXNUM")
    (expect (type-to-string type-node) :to-equal expected)))

(it-sequential "printer-primitive-types string"
  (destructuring-bind (type-node expected) (list type-string "STRING")
    (expect (type-to-string type-node) :to-equal expected)))

(it-sequential "printer-var-contains-substring named"
  (destructuring-bind (type-node expected-sub) (list (fresh-type-var :name  "alpha") "alpha")
    (expect (search expected-sub (type-to-string type-node)) :to-be-truthy)))

(it-sequential "printer-var-contains-substring rigid"
  (destructuring-bind (type-node expected-sub) (list (fresh-rigid-var "test") "sk")
    (expect (search expected-sub (type-to-string type-node)) :to-be-truthy)))

(it-sequential "printer-linked-type-var-shows-resolved-type"
  (let ((v (fresh-type-var :name "a")))
    (setf (cl-cc/type:type-var-link v) type-int)
    (expect (type-to-string v) :to-equal "FIXNUM")))

(it-sequential "printer-type-scheme-produces-non-empty-string"
  (let* ((v (fresh-type-var :name "a"))
         (scheme (make-type-scheme (list v) v))
         (s (type-to-string scheme)))
    (expect (> (length s) 0) :to-be-truthy)))

;;; ─── type-to-string: composite types ───────────────────────────────────────

(it-sequential "printer-arrow-cases single-param"
  (destructuring-bind (params ret effects expected-sub expected-sub2) (list (list type-int) type-string nil "FIXNUM" "STRING")
    (let ((s (type-to-string (make-type-arrow params ret :effects effects))))
    (expect (search "->" s) :to-be-truthy)
    (assert-when-present expected-sub  (expect (search expected-sub  s) :to-be-truthy))
    (assert-when-present expected-sub2 (expect (search expected-sub2 s) :to-be-truthy))
    (assert-when-present effects       (expect (search "IO" s) :to-be-truthy)))))

(it-sequential "printer-arrow-cases multi-param"
  (destructuring-bind (params ret effects expected-sub expected-sub2) (list (list type-int type-string) type-bool nil nil nil)
    (let ((s (type-to-string (make-type-arrow params ret :effects effects))))
    (expect (search "->" s) :to-be-truthy)
    (assert-when-present expected-sub  (expect (search expected-sub  s) :to-be-truthy))
    (assert-when-present expected-sub2 (expect (search expected-sub2 s) :to-be-truthy))
    (assert-when-present effects       (expect (search "IO" s) :to-be-truthy)))))

(it-sequential "printer-arrow-cases with-effects"
  (destructuring-bind (params ret effects expected-sub expected-sub2) (list (list type-int) type-string +io-effect-row+ "IO" nil)
    (let ((s (type-to-string (make-type-arrow params ret :effects effects))))
    (expect (search "->" s) :to-be-truthy)
    (assert-when-present expected-sub  (expect (search expected-sub  s) :to-be-truthy))
    (assert-when-present expected-sub2 (expect (search expected-sub2 s) :to-be-truthy))
    (assert-when-present effects       (expect (search "IO" s) :to-be-truthy)))))

(it-sequential "printer-container-type-delimiters product"
  (destructuring-bind (ty expected-sub) (list (make-type-product :elems (list type-int type-string)) ",")
    (expect (search expected-sub (type-to-string ty)) :to-be-truthy)))

(it-sequential "printer-container-type-delimiters variant"
  (destructuring-bind (ty expected-sub) (list (make-type-variant :cases (list (cons 'some type-int) (cons 'none type-null))
                                         :row-var nil) "<")
    (expect (search expected-sub (type-to-string ty)) :to-be-truthy)))

(it-sequential "printer-container-type-delimiters type-app"
  (destructuring-bind (ty expected-sub) (list (make-type-app :fun type-int :arg type-string) "(")
    (expect (search expected-sub (type-to-string ty)) :to-be-truthy)))

(it-sequential "printer-record-cases closed"
  (destructuring-bind (open-p expected-bracket expected-field) (list nil "{" "x")
    (let* ((rv (when open-p (fresh-type-var :name "rho")))
         (fields (if open-p
                     (list (cons 'x type-int))
                     (list (cons 'x type-int) (cons 'y type-string))))
         (r (make-type-record :fields fields :row-var rv))
         (s (type-to-string r)))
    (expect (search expected-bracket s) :to-be-truthy)
    (assert-when-present expected-field
      (expect (search expected-field (string-downcase s)) :to-be-truthy)))))

(it-sequential "printer-record-cases open"
  (destructuring-bind (open-p expected-bracket expected-field) (list t "|" nil)
    (let* ((rv (when open-p (fresh-type-var :name "rho")))
         (fields (if open-p
                     (list (cons 'x type-int))
                     (list (cons 'x type-int) (cons 'y type-string))))
         (r (make-type-record :fields fields :row-var rv))
         (s (type-to-string r)))
    (expect (search expected-bracket s) :to-be-truthy)
    (assert-when-present expected-field
      (expect (search expected-field (string-downcase s)) :to-be-truthy)))))

(it-sequential "printer-binary-separator-types union"
  (destructuring-bind (ty expected-sep) (list (make-type-union        (list type-int type-string)) "|")
    (expect (search expected-sep (type-to-string ty)) :to-be-truthy)))

(it-sequential "printer-binary-separator-types intersection"
  (destructuring-bind (ty expected-sep) (list (make-type-intersection (list type-int type-string)) "&")
    (expect (search expected-sep (type-to-string ty)) :to-be-truthy)))

(it-sequential "printer-quantified-types forall"
  (destructuring-bind (ty) (list (let ((v (fresh-type-var :name "a"))) (make-type-forall :var v :body type-int)))
    (expect (> (length (type-to-string ty)) 0) :to-be-truthy)))

(it-sequential "printer-quantified-types exists"
  (destructuring-bind (ty) (list (let ((v (fresh-type-var :name "a"))) (make-type-exists :var v :body type-int)))
    (expect (> (length (type-to-string ty)) 0) :to-be-truthy)))

(it-sequential "printer-binder-types"
  (let ((v (fresh-type-var :name "a")))
    (expect (> (length (type-to-string (cl-cc/type:make-type-lambda :var v :knd nil :body type-int))) 0) :to-be-truthy)
    (expect (> (length (type-to-string (make-type-mu :var v :body v))) 0) :to-be-truthy)))

(it-sequential "printer-wrapper-type-annotations refinement"
  (destructuring-bind (ty expected-fragment) (list (cl-cc/type:make-type-refinement :base type-int :predicate nil) "<pred>")
    (expect (search expected-fragment (type-to-string ty)) :to-be-truthy)))

(it-sequential "printer-wrapper-type-annotations linear"
  (destructuring-bind (ty expected-fragment) (list (make-type-linear :base type-int :grade :one) "1")
    (expect (search expected-fragment (type-to-string ty)) :to-be-truthy)))

(it-sequential "printer-wrapper-type-annotations capability"
  (destructuring-bind (ty expected-fragment) (list (cl-cc/type:make-type-capability :base type-int :cap 'read) "READ")
    (expect (search expected-fragment (type-to-string ty)) :to-be-truthy)))

(it-sequential "printer-handler-uses-bracket-notation"
  (let* ((eff (make-type-effect-op :name 'io :args nil))
         (h (cl-cc/type:make-type-handler :effect eff :input type-int :output type-string))
         (s (type-to-string h)))
    (expect (search "[" s) :to-be-truthy)
    (expect (search "=>" s) :to-be-truthy)))

(it-sequential "printer-gadt-constructor-includes-double-colon"
  (let* ((gc (cl-cc/type:make-type-gadt-con
              :name 'just :arg-types (list type-int) :index-type type-any))
         (s (type-to-string gc)))
    (expect (search "::" s) :to-be-truthy)))

;;; ─── type-to-string: effect rows ──────────────────────────────────────────

(it-sequential "printer-effect-rows-and-ops"
  (expect (type-to-string +pure-effect-row+) :to-equal "{}")
  (expect (search "IO" (type-to-string +io-effect-row+)) :to-be-truthy)
  (let* ((rv (fresh-type-var :name "e"))
         (er (make-type-effect-row :effects nil :row-var rv)))
    (expect (search "|" (type-to-string er)) :to-be-truthy))
  (let* ((op (make-type-effect-op :name 'state :args (list type-int)))
         (s  (type-to-string op)))
    (expect (search "STATE"  s) :to-be-truthy)
    (expect (search "FIXNUM" s) :to-be-truthy))
  (let* ((rv  (fresh-type-var :name 'epsilon))
         (row (make-type-effect-row
               :effects (list (make-type-effect-op :name 'io))
               :row-var rv))
         (s (type-to-string row)))
    (expect (search "IO" (string-upcase s)) :to-be-truthy)
    (expect (search "|" s) :to-be-truthy)))

;;; ─── type-to-string: constraint / qualified ───────────────────────────────

(it-sequential "printer-constraint-and-qualified"
  (let ((tc (cl-cc/type:make-type-constraint :class-name 'eq :type-arg type-int)))
    (expect (search "EQ" (type-to-string tc)) :to-be-truthy)
    (let ((s (type-to-string (make-type-qualified :constraints (list tc) :body type-int))))
      (expect (search "=>" s) :to-be-truthy))
    (let ((s (type-to-string (make-type-qualified :constraints nil :body type-int))))
      (expect s :to-equal "FIXNUM"))))

;;; ─── unparse-type roundtrip ────────────────────────────────────────────────

(it-sequential "unparse-type-forms"
  (expect (cl-cc/type:unparse-type type-int) :to-be 'fixnum)
  (expect (first (cl-cc/type:unparse-type (make-type-arrow (list type-int) type-string))) :to-be 'cl-cc/type::->)
  (expect (first (cl-cc/type:unparse-type (make-type-union (list type-int type-string)))) :to-be 'or)
  (expect (first (cl-cc/type:unparse-type (make-type-product :elems (list type-int)))) :to-be 'values))

;;; ─── type-to-string: edge cases not covered above ────────────────────────

(it-sequential "printer-atomic-sentinel-strings nil-val"
  (destructuring-bind (expected node) (list "NIL" nil)
    (expect (type-to-string node) :to-equal expected)))

(it-sequential "printer-atomic-sentinel-strings unknown"
  (destructuring-bind (expected node) (list "?" cl-cc/type:+type-unknown+)
    (expect (type-to-string node) :to-equal expected)))

(it-sequential "printer-unnamed-var-format type-var"
  (destructuring-bind (node expected-sub forbidden-sub) (list (fresh-type-var :name nil) "?t" nil)
    (let ((s (type-to-string node)))
    (expect (search expected-sub s) :to-be-truthy)
    (when forbidden-sub (expect (search forbidden-sub s) :to-be-falsy)))))

(it-sequential "printer-unnamed-var-format rigid-var"
  (destructuring-bind (node expected-sub forbidden-sub) (list (fresh-rigid-var nil) "sk" "[")
    (let ((s (type-to-string node)))
    (expect (search expected-sub s) :to-be-truthy)
    (when forbidden-sub (expect (search forbidden-sub s) :to-be-falsy)))))

(it-sequential "printer-fallback-hash-table"
  (expect (search "#<type" (type-to-string (make-hash-table))) :to-be-truthy))

(it-sequential "printer-looks-like-type-specifier-p fixnum"
  (destructuring-bind (expected form) (list t 'fixnum)
    (if expected
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "printer-looks-like-type-specifier-p string"
  (destructuring-bind (expected form) (list t 'string)
    (if expected
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "printer-looks-like-type-specifier-p int-shorthand"
  (destructuring-bind (expected form) (list t 'cl-cc/type::int)
    (if expected
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "printer-looks-like-type-specifier-p bool-shorthand"
  (destructuring-bind (expected form) (list t 'cl-cc/type::bool)
    (if expected
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "printer-looks-like-type-specifier-p or-composite"
  (destructuring-bind (expected form) (list t '(or fixnum string))
    (if expected
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "printer-looks-like-type-specifier-p and-composite"
  (destructuring-bind (expected form) (list t '(and fixnum string))
    (if expected
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "printer-looks-like-type-specifier-p bang-prefix"
  (destructuring-bind (expected form) (list t '(!linear fixnum))
    (if expected
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "printer-looks-like-type-specifier-p frobnitz-sym"
  (destructuring-bind (expected form) (list nil 'frobnitz)
    (if expected
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "printer-looks-like-type-specifier-p frobnitz-list"
  (destructuring-bind (expected form) (list nil '(frobnitz fixnum))
    (if expected
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
      (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "printer-arrow-mult-table zero"
  (destructuring-bind (mult expected) (list :zero "-0->")
    (let ((arr (make-type-arrow (list type-int) type-string :mult mult)))
    (expect (search expected (type-to-string arr)) :to-be-truthy))))

(it-sequential "printer-arrow-mult-table one"
  (destructuring-bind (mult expected) (list :one "-1->")
    (let ((arr (make-type-arrow (list type-int) type-string :mult mult)))
    (expect (search expected (type-to-string arr)) :to-be-truthy))))

(it-sequential "printer-arrow-mult-table omega"
  (destructuring-bind (mult expected) (list :omega "->")
    (let ((arr (make-type-arrow (list type-int) type-string :mult mult)))
    (expect (search expected (type-to-string arr)) :to-be-truthy))))

(it-sequential "printer-type-error-sentinel"
  (let ((e1 (make-type-error :message "unbound x"))
        (e2 (make-type-error :message "unknown")))
    (expect (type-to-string e1) :to-equal "<error: unbound x>")
    (expect (type-to-string e2) :to-equal "?")
    (expect (type-to-string cl-cc/type:+type-unknown+) :to-equal "?")))

(it-sequential "printer-compound-types"
  (let ((pair (make-type-product :elems (list type-int type-string))))
    (expect (type-to-string pair) :to-equal "(FIXNUM, STRING)"))
  (let ((closed (make-type-record :fields (list (cons 'x type-int)
                                                (cons 'y type-bool))
                                  :row-var nil)))
    (let ((s (type-to-string closed)))
      (expect (search "X" (string-upcase s)) :to-be-truthy)
      (expect (search "Y" (string-upcase s)) :to-be-truthy)))
  (let ((open (make-type-record :fields (list (cons 'x type-int))
                                :row-var (fresh-type-var :name 'rho))))
    (expect (search "|" (type-to-string open)) :to-be-truthy))
  (let ((lin (make-type-linear :base type-int :grade :one)))
    (let ((s (type-to-string lin)))
      (expect (search "1" s) :to-be-truthy)
      (expect (search "FIXNUM" s) :to-be-truthy))))

(it-sequential "printer-unicode-type-operators forall"
  (destructuring-bind (glyph ty) (list "∀" (let* ((a  (fresh-type-var :name 'a))
                                 (fn (make-type-arrow (list a) a)))
                            (make-type-forall :var a :body fn)))
    (expect (search glyph (type-to-string ty)) :to-be-truthy)))

(it-sequential "printer-unicode-type-operators mu"
  (destructuring-bind (glyph ty) (list "μ" (let* ((a (fresh-type-var :name 'a)))
                            (make-type-mu :var a :body (make-type-union (list type-null a)))))
    (expect (search glyph (type-to-string ty)) :to-be-truthy)))

;;; ─── list-interleave ───────────────────────────────────────────────────────

(it-sequential "list-interleave-behavior"
  (expect (cl-cc/type::list-interleave '(a b c) 'x) :to-equal '(a x b x c))
  (expect (cl-cc/type::list-interleave '(a) 'x) :to-equal '(a))
  (expect (cl-cc/type::list-interleave nil 'x) :to-be-null))
