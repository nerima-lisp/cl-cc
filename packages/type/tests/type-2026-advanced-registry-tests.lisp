;;;; tests/type-2026-advanced-registry-tests.lisp - 2026 Advanced Feature Registry Tests
;;;;
;;;; Covers: advanced feature FR-id registry, implementation evidence, semantic completion,
;;;; contract registry, parser roundtrips, head aliases, and structural traversal tests.

(in-package :cl-cc/test)


(defun %advanced-doc-fr-ids ()
  "Return the ordered FR headings from docs/type-advanced.md."
  (let ((ids nil))
    (dolist (line (uiop:split-string
                   (uiop:read-file-string
                    (merge-pathnames #P"docs/type-advanced.md" (uiop:getcwd)))
                   :separator '(#\Newline))
              (nreverse ids))
      (let ((fr-pos (and (>= (length line) 7)
                         (string= "### " (subseq line 0 4))
                         (search "FR-" line))))
        (when (and fr-pos
                   (<= (+ fr-pos 7) (length line)))
          (push (subseq line fr-pos (+ fr-pos 7)) ids))))))

(defun %cl-weave-anchor-registered-p (anchor)
  "Return T when ANCHOR names a cl-weave-registered test (it-sequential). Walks
cl-weave's root suite and matches the anchor by its lowercased name (exact, or
\"anchor \" prefix for deftest-each cases)."
  (let ((target (string-downcase (symbol-name anchor)))
        (root (ignore-errors (cl-weave::root-suite))))
    (when root
      (labels ((walk (node)
                 (if (cl-weave::suite-p node)
                     (some #'walk (cl-weave::suite-children node))
                     (let ((name (cl-weave::test-case-name node)))
                       (and (stringp name)
                            (or (string-equal name target)
                                (let ((p (concatenate 'string target " ")))
                                  (and (>= (length name) (length p))
                                       (string-equal (subseq name 0 (length p)) p)))))))))
        (walk root)))))

(defun %test-anchor-registered-p (anchor)
  "Return T when ANCHOR names a registered test, in cl-weave's suite tree
(it-sequential) or the legacy cl-cc/test *KNOWN-TEST-NAMES* registry."
  (or (%cl-weave-anchor-registered-p anchor)
      (let ((test-symbol (find-symbol (symbol-name anchor) :cl-cc/test)))
        (or (and test-symbol
                 (nth-value 1 (gethash test-symbol cl-cc/test::*known-test-names*)))
            (let ((case-prefix (concatenate 'string "/" (symbol-name anchor) " [")))
              (loop for name being the hash-keys of cl-cc/test::*known-test-names*
                    thereis (search case-prefix (symbol-name name))))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Advanced feature registry / representation tests
;;; ─────────────────────────────────────────────────────────────────────────


(it-sequential "advanced-feature-registry-covers-doc-fr-list"
  (let ((expected-ids (%advanced-doc-fr-ids))
        (actual-ids (cl-cc/type:list-type-advanced-feature-ids))
        (contract-count (hash-table-count cl-cc/type::*type-advanced-contract-registry*))
        (evidence-count (hash-table-count cl-cc/type::*type-advanced-implementation-evidence-registry*)))
    (expect (= (length expected-ids) (length actual-ids)) :to-be-truthy)
    (expect (= (length expected-ids) contract-count) :to-be-truthy)
    (expect (= (length expected-ids) evidence-count) :to-be-truthy)
    (expect actual-ids :to-equal expected-ids)
    (dolist (feature-id expected-ids)
      (expect (cl-cc/type:lookup-type-advanced-feature feature-id) :to-be-truthy)
      (expect (cl-cc/type:type-advanced-semantics-implemented-p feature-id) :to-be-truthy)
      (expect (cl-cc/type::lookup-type-advanced-contract feature-id) :to-be-truthy)
      (expect (cl-cc/type::lookup-type-advanced-implementation-evidence feature-id) :to-be-truthy))))

(it-sequential "advanced-feature-implementation-evidence-covers-all-fr-ids"
  (let* ((expected-ids (mapcar #'first cl-cc/type:+type-advanced-feature-specs+))
         (table cl-cc/type::*type-advanced-implementation-evidence-registry*)
         (actual-ids nil))
    (maphash (lambda (feature-id evidence)
               (declare (ignore evidence))
               (push feature-id actual-ids))
             table)
    (setf actual-ids (sort actual-ids #'string<))
    (expect (= (length expected-ids) (hash-table-count table)) :to-be-truthy)
    (expect actual-ids :to-equal expected-ids)
    (dolist (feature-id expected-ids)
      (let ((evidence (cl-cc/type::lookup-type-advanced-implementation-evidence feature-id)))
        (expect evidence :to-be-truthy)
        (expect (consp (cl-cc/type::type-advanced-implementation-evidence-modules evidence)) :to-be-truthy)
        (expect (consp (cl-cc/type::type-advanced-implementation-evidence-api-symbols evidence)) :to-be-truthy)
        (expect (consp (cl-cc/type::type-advanced-implementation-evidence-test-anchors evidence)) :to-be-truthy)
        (dolist (module (cl-cc/type::type-advanced-implementation-evidence-modules evidence))
          (expect (cl-cc/type::%type-advanced-implementation-module-present-p module) :to-be-truthy))
        (dolist (api (cl-cc/type::type-advanced-implementation-evidence-api-symbols evidence))
          (expect (fboundp api) :to-be-truthy))
        (dolist (anchor (cl-cc/type::type-advanced-implementation-evidence-test-anchors evidence))
          (expect (%test-anchor-registered-p anchor) :to-be-truthy))
        (expect (cl-cc/type::%type-advanced-implementation-evidence-complete-p evidence) :to-be-truthy)))))

(it-sequential "advanced-feature-semantic-completion-requires-implementation-evidence"
  (let* ((cl-cc/type::*type-advanced-implementation-evidence-registry*
           (%copy-hash-table-shallow
            cl-cc/type::*type-advanced-implementation-evidence-registry*))
         (feature-id "FR-2501")
         (contract (cl-cc/type::lookup-type-advanced-contract feature-id))
         (table cl-cc/type::*type-advanced-implementation-evidence-registry*)
         (saved (cl-cc/type::lookup-type-advanced-implementation-evidence feature-id))
         (modules (cl-cc/type::type-advanced-implementation-evidence-modules saved))
         (api-symbols (cl-cc/type::type-advanced-implementation-evidence-api-symbols saved))
         (test-anchors (cl-cc/type::type-advanced-implementation-evidence-test-anchors saved)))
    (expect contract :to-be-truthy)
    (expect saved :to-be-truthy)
    (expect (cl-cc/type:type-advanced-semantics-implemented-p feature-id) :to-be-truthy)
    (unwind-protect
        (progn
          (remhash feature-id table)
          (expect (cl-cc/type::lookup-type-advanced-contract feature-id) :to-be-truthy)
          (expect (cl-cc/type::lookup-type-advanced-implementation-evidence feature-id) :to-be-falsy)
          (expect (cl-cc/type:type-advanced-semantics-implemented-p feature-id) :to-be-falsy)
          (expect (cl-cc/type:type-advanced-valid-p
                        (cl-cc/type:make-type-dynamic cl-cc/type:type-int)) :to-be-truthy))
      (setf (gethash feature-id table) saved))
    (unwind-protect
        (progn
          (setf (gethash feature-id table)
                (cl-cc/type::%make-type-advanced-implementation-evidence
                 :feature-id feature-id
                 :modules modules
                 :api-symbols nil
                 :test-anchors test-anchors
                 :summary "missing API symbols should not satisfy completion"))
          (expect (cl-cc/type:type-advanced-semantics-implemented-p feature-id) :to-be-falsy)
          (setf (gethash feature-id table)
                (cl-cc/type::%make-type-advanced-implementation-evidence
                 :feature-id feature-id
                 :modules modules
                 :api-symbols '(cl-cc/type::definitely-missing-advanced-api)
                 :test-anchors test-anchors
                 :summary "unbound API symbols should not satisfy completion"))
          (expect (cl-cc/type:type-advanced-semantics-implemented-p feature-id) :to-be-falsy)
          (setf (gethash feature-id table)
                (cl-cc/type::%make-type-advanced-implementation-evidence
                 :feature-id feature-id
                 :modules modules
                 :api-symbols api-symbols
                 :test-anchors nil
                 :summary "missing test anchors should not satisfy completion"))
          (expect (cl-cc/type:type-advanced-semantics-implemented-p feature-id) :to-be-falsy)
          (setf (gethash feature-id table)
                (cl-cc/type::%make-type-advanced-implementation-evidence
                 :feature-id feature-id
                 :modules modules
                 :api-symbols api-symbols
                 :test-anchors '(cl-cc/test::definitely-missing-advanced-test-anchor)
                 :summary "unregistered test anchors should not satisfy completion"))
          (expect (cl-cc/type:type-advanced-semantics-implemented-p feature-id) :to-be-falsy))
      (setf (gethash feature-id table) saved))
    (expect (cl-cc/type:type-advanced-semantics-implemented-p feature-id) :to-be-truthy)))


(it-sequential "advanced-feature-contract-registry-rejects-unknown-and-missing-contracts"
  (let ((cl-cc/type::*type-advanced-contract-registry*
          (%copy-hash-table-shallow
           cl-cc/type::*type-advanced-contract-registry*)))
    (expect (cl-cc/type:type-advanced-semantics-implemented-p "FR-9999") :to-be-falsy)
    (signals error (cl-cc/type:parse-type-specifier '(advanced fr-9999 integer)))
    (let* ((feature-id "FR-1606")
           (table cl-cc/type::*type-advanced-contract-registry*)
           (saved (cl-cc/type::lookup-type-advanced-contract feature-id)))
      (expect saved :to-be-truthy)
      (unwind-protect
           (progn
             (remhash feature-id table)
             (expect (cl-cc/type:type-advanced-semantics-implemented-p feature-id) :to-be-falsy)
             (signals error (cl-cc/type:make-type-advanced
                  :feature-id feature-id
                  :args (list 'cache-entry)
                  :properties (list (cons :dependency-graph 'call-graph)
                                    (cons :cache 'module-cache)))))
        (setf (gethash feature-id table) saved))
      (expect (cl-cc/type:type-advanced-semantics-implemented-p feature-id) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips generic"
  (destructuring-bind (form expected-id printed-head) (list '(advanced fr-1503 (secret string) :flow secret) "FR-1503" "ADVANCED")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips type-safe-ffi"
  (destructuring-bind (form expected-id printed-head) (list '(type-safe-ffi (c-callback (-> fixnum fixnum)) :abi c) "FR-2103" "TYPE-SAFE-FFI")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips future"
  (destructuring-bind (form expected-id printed-head) (list '(future fixnum :mode eager) "FR-2201" "FUTURE")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips units-of-measure"
  (destructuring-bind (form expected-id printed-head) (list '(units-of-measure float :unit meter) "FR-2302" "UNITS-OF-MEASURE")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips dynamic"
  (destructuring-bind (form expected-id printed-head) (list '(dynamic fixnum) "FR-2501" "DYNAMIC")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips typerep"
  (destructuring-bind (form expected-id printed-head) (list '(typerep (list fixnum)) "FR-2502" "TYPEREP")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips dict"
  (destructuring-bind (form expected-id printed-head) (list '(dict (eq fixnum)) "FR-2603" "DICT")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips mapped-type"
  (destructuring-bind (form expected-id printed-head) (list '(mapped-type (list fixnum) :transform optional) "FR-3301" "MAPPED-TYPE")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips readonly"
  (destructuring-bind (form expected-id printed-head) (list '(readonly (list fixnum)) "FR-3303" "READONLY")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips partial-type"
  (destructuring-bind (form expected-id printed-head) (list '(partial-type (list fixnum)) "FR-3304" "PARTIAL-TYPE")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips api-type"
  (destructuring-bind (form expected-id printed-head) (list '(api-type (get "/users" (list fixnum))) "FR-3305" "API-TYPE")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips linear-logic"
  (destructuring-bind (form expected-id printed-head) (list '(linear-logic tensor fixnum string) "FR-3101" "LINEAR-LOGIC")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips algebraic-subtype"
  (destructuring-bind (form expected-id printed-head) (list '(algebraic-subtype (-> fixnum string)) "FR-3201" "ALGEBRAIC-SUBTYPE")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips brand-type"
  (destructuring-bind (form expected-id printed-head) (list '(brand-type user-id string) "FR-3205" "BRAND-TYPE")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips qtt"
  (destructuring-bind (form expected-id printed-head) (list '(qtt 0 (vector fixnum)) "FR-3401" "QTT")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips graded"
  (destructuring-bind (form expected-id printed-head) (list '(graded :omega (list fixnum)) "FR-3402" "GRADED")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-parser-roundtrips open-union"
  (destructuring-bind (form expected-id printed-head) (list '(open-union (io state) fixnum) "FR-3404" "OPEN-UNION")
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal expected-id)
    (let ((roundtrip (cl-cc/type:unparse-type ty)))
      (expect (cl-cc/type:looks-like-type-specifier-p roundtrip) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p ty (cl-cc/type:parse-type-specifier roundtrip)) :to-be-truthy))
    (expect (search printed-head (cl-cc/type:type-to-string ty)) :to-be-truthy))))

(it-sequential "advanced-feature-head-registry-covers-representative-aliases"
  (expect (cl-cc/type:type-advanced-feature-id-for-head 'dynamic) :to-equal "FR-2501")
  (expect (cl-cc/type:type-advanced-feature-id-for-head 'typerep) :to-equal "FR-2502")
  (expect (cl-cc/type:type-advanced-feature-id-for-head 'api-type) :to-equal "FR-3305")
  (expect (cl-cc/type:type-advanced-head-p 'advanced) :to-be-truthy)
  (expect (cl-cc/type:type-advanced-head-p 'mapped-type) :to-be-truthy))

(it-sequential "advanced-null-safety-option-parses-nullable-union"
  (let ((ty (cl-cc/type:parse-type-specifier '(option string))))
    (expect (cl-cc/type:type-union-p ty) :to-be-truthy)
    (expect (some (lambda (member)
                         (cl-cc/type:type-equal-p member cl-cc/type:type-null))
                       (cl-cc/type:type-union-types ty)) :to-be-truthy)
    (expect (some (lambda (member)
                         (cl-cc/type:type-equal-p member cl-cc/type:type-string))
                       (cl-cc/type:type-union-types ty)) :to-be-truthy)))

(it-sequential "advanced-information-flow-enforces-security-lattice"
  (expect (cl-cc/type:type-advanced-security-label<= :public :secret) :to-be-truthy)
  (expect (cl-cc/type:type-advanced-security-label<= :secret :public) :to-be-falsy)
  (signals error (cl-cc/type:parse-type-specifier '(advanced fr-1503 (secret string) :flow public)))
  (let ((ty (cl-cc/type:parse-type-specifier
             '(advanced fr-1503 (secret string) :flow public :evidence (declassify audit-log)))))
    (expect (cl-cc/type:type-advanced-valid-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal "FR-1503"))
  (let ((public-string (cl-cc/type:parse-type-specifier
                        '(advanced fr-1503 (public string) :flow public)))
        (secret-string (cl-cc/type:parse-type-specifier
                        '(advanced fr-1503 (secret string) :flow secret))))
    (expect (cl-cc/type:is-subtype-p public-string secret-string) :to-be-truthy)
    (expect (cl-cc/type:is-subtype-p secret-string public-string) :to-be-falsy)))

(it-sequential "advanced-validation-rejects-malformed-units-routes-and-ffi"
  (signals error (cl-cc/type:parse-type-specifier '(units-of-measure float)))
  (signals error (cl-cc/type:parse-type-specifier '(api-type (fetch "/users" string))))
  (signals error (cl-cc/type:parse-type-specifier '(type-safe-ffi (c-ptr))))
  (let ((route (cl-cc/type:parse-type-specifier '(api-type (get "/users/{id}" integer user)))))
    (expect (cl-cc/type:type-advanced-valid-p route) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-route-p (first (cl-cc/type:type-advanced-args route))) :to-be-truthy)))

(it-sequential "advanced-proof-like-features-require-evidence"
  (signals error (cl-cc/type:parse-type-specifier '(advanced fr-2002 safe-div-proof)))
  (let ((pcc (cl-cc/type:parse-type-specifier
              '(advanced fr-2002 safe-div-proof :evidence (proof non-zero-denominator)))))
    (expect (cl-cc/type:type-advanced-valid-p pcc) :to-be-truthy)
    (expect (cl-cc/type:type-advanced-feature-id pcc) :to-equal "FR-2002")))

(it-sequential "advanced-graded-and-branded-types-have-semantic-shape"
  (signals error (cl-cc/type:parse-type-specifier '(graded :sometimes (list fixnum))))
  (let ((user-id (cl-cc/type:parse-type-specifier '(brand-type user-id string)))
        (post-id (cl-cc/type:parse-type-specifier '(brand-type post-id string))))
    (expect (cl-cc/type:type-advanced-valid-p user-id) :to-be-truthy)
    (expect (cl-cc/type:type-equal-p user-id post-id) :to-be-falsy)))

(it-sequential "advanced-node-children-and-free-vars-follow-nested-type-payload"
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (b (cl-cc/type:fresh-type-var 'b))
         (node (cl-cc/type:make-type-advanced
                 :feature-id "FR-1601"
                 :name 'typed-hole
                 :args (list a (cl-cc/type:make-type-arrow (list b) cl-cc/type:type-int)))))
    (let ((children (cl-cc/type:type-children node))
          (free-vars (cl-cc/type:type-free-vars node)))
      (expect (= 2 (length children)) :to-be-truthy)
      (expect (cl-cc/type:type-var-p (first children)) :to-be-truthy)
      (expect (cl-cc/type:type-arrow-p (second children)) :to-be-truthy)
      (expect (= 2 (length free-vars)) :to-be-truthy)
      (expect (some (lambda (var) (cl-cc/type:type-var-equal-p var a)) free-vars) :to-be-truthy)
      (expect (some (lambda (var) (cl-cc/type:type-var-equal-p var b)) free-vars) :to-be-truthy))))

(it-sequential "advanced-node-zonk-updates-args-properties-and-evidence"
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (b (cl-cc/type:fresh-type-var 'b))
         (subst (cl-cc/type:make-substitution))
         (node (cl-cc/type:make-type-advanced
                :feature-id "FR-2501"
                :name 'dynamic
                :args (list (cl-cc/type:make-type-arrow (list a) b))
                :properties (list (cons :guard a))
                :evidence (list 'proof b))))
    (cl-cc/type:subst-extend! a cl-cc/type:type-int subst)
    (cl-cc/type:subst-extend! b cl-cc/type:type-string subst)
    (expect (cl-cc/type:type-equal-p
      (cl-cc/type:make-type-advanced
       :feature-id "FR-2501"
       :name 'dynamic
       :args (list (cl-cc/type:make-type-arrow (list cl-cc/type:type-int) cl-cc/type:type-string))
       :properties (list (cons :guard cl-cc/type:type-int))
       :evidence (list 'proof cl-cc/type:type-string))
      (cl-cc/type:zonk node subst)) :to-be-truthy)))

(it-sequential "advanced-node-unification-is-structural-and-fr-scoped"
  (let ((a (cl-cc/type:fresh-type-var 'a)))
    (multiple-value-bind (subst ok)
        (cl-cc/type:type-unify (cl-cc/type:make-type-dynamic a)
                               (cl-cc/type:parse-type-specifier '(advanced fr-2501 fixnum)))
      (expect ok :to-be-truthy)
      (expect (cl-cc/type:substitution-p subst) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p cl-cc/type:type-int (cl-cc/type:zonk a subst)) :to-be-truthy)))
  (multiple-value-bind (_ ok)
      (cl-cc/type:type-unify (cl-cc/type:parse-type-specifier '(dynamic fixnum))
                             (cl-cc/type:parse-type-specifier '(typerep fixnum)))
    (declare (ignore _))
    (expect ok :to-be-falsy)))

(it-sequential "advanced-node-properties-are-key-order-insensitive"
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (left (cl-cc/type:make-type-advanced
                :feature-id "FR-2501"
                :name 'dynamic
                :args (list a)
                :properties (list (cons :guard a)
                                  (cons :mode 'checked))))
         (right (cl-cc/type:make-type-advanced
                 :feature-id "FR-2501"
                 :name 'dynamic
                 :args (list cl-cc/type:type-int)
                 :properties (list (cons :mode 'checked)
                                   (cons :guard cl-cc/type:type-int)))))
    (multiple-value-bind (subst ok)
        (cl-cc/type:type-unify left right)
      (expect ok :to-be-truthy)
      (expect (cl-cc/type:type-equal-p cl-cc/type:type-int
                                            (cl-cc/type:zonk a subst)) :to-be-truthy)
      (expect (cl-cc/type:type-equal-p right
                                            (cl-cc/type:zonk left subst)) :to-be-truthy))))

(it-sequential "advanced-node-subtyping-degrades-safely"
  (let ((dynamic-fixnum (cl-cc/type:parse-type-specifier '(dynamic fixnum)))
        (dynamic-string (cl-cc/type:parse-type-specifier '(dynamic string)))
        (generic-fixnum (cl-cc/type:parse-type-specifier '(advanced fr-2501 fixnum))))
    (expect (cl-cc/type:is-subtype-p dynamic-fixnum generic-fixnum) :to-be-truthy)
    (expect (cl-cc/type:is-subtype-p cl-cc/type:type-string dynamic-fixnum) :to-be-falsy)
    (expect (cl-cc/type:is-subtype-p dynamic-fixnum dynamic-string) :to-be-falsy)
    (expect (cl-cc/type:is-subtype-p dynamic-fixnum cl-cc/type:type-any) :to-be-truthy)))
