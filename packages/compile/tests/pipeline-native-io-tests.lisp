;;;; tests/unit/compile/pipeline-native-io-tests.lisp — Pipeline Native I/O + Integration
;;;;
;;;; Tests for %copy-file-bytes, compile-file-to-native cache hits, typeclass
;;;; macro registration, and %cps-native-compile-safe-ast-p.
;;;; Suite: pipeline-native-suite (defined in pipeline-native-tests.lisp).

(in-package :cl-cc/test)

(in-suite pipeline-native-suite)

(defmacro with-native-cache-stubs ((&key cache-path) &body body)
  "Stub cache-related native helper calls for routing/cache tests."
  `(with-replaced-function (cl-cc::%compile-cache-key
                            (lambda (&rest args)
                              (declare (ignore args))
                              "cache-key"))
     (with-replaced-function (cl-cc::%compile-cache-path
                              (lambda (&rest args)
                                (declare (ignore args))
                                ,cache-path))
       (with-replaced-function (cl-cc::%copy-file-bytes
                                (lambda (from to)
                                  (declare (ignore from))
                                  to))
         ,@body))))

;;; ─── %copy-file-bytes ───────────────────────────────────────────────────────

(deftest pipeline-native-copy-file-bytes-returns-dst-and-creates-file
  "%copy-file-bytes returns destination pathname and creates destination file."
  (uiop:with-temporary-file (:pathname src :type "bin")
    (uiop:with-temporary-file (:pathname dst :type "bin" :keep t)
      (let* ((data (make-array 4 :element-type '(unsigned-byte 8)
                                :initial-contents '(1 2 3 4)))
             (_ (with-open-file (out src :direction :output
                                        :if-exists :supersede
                                        :element-type '(unsigned-byte 8))
                  (write-sequence data out)))
             (result (cl-cc::%copy-file-bytes src dst)))
        (declare (ignore _))
        (assert-true (pathnamep result))
        (assert-equal (namestring dst) (namestring result))
        (assert-true (probe-file dst))
        (ignore-errors (delete-file dst))))))

(deftest pipeline-native-copy-file-bytes-same-contents
  "%copy-file-bytes produces a destination file with identical contents."
  (uiop:with-temporary-file (:pathname src :type "bin")
    (uiop:with-temporary-file (:pathname dst :type "bin" :keep t)
      (let ((data (make-array 8 :element-type '(unsigned-byte 8)
                               :initial-contents '(0 1 2 3 255 128 64 32))))
        (with-open-file (out src :direction :output
                                 :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
          (write-sequence data out))
        (cl-cc::%copy-file-bytes src dst)
        (let ((read-back (make-array 8 :element-type '(unsigned-byte 8))))
          (with-open-file (in dst :direction :input
                                  :element-type '(unsigned-byte 8))
            (read-sequence read-back in))
          (assert-equal (coerce data 'list)
                        (coerce read-back 'list)))
        (ignore-errors (delete-file dst))))))

(deftest pipeline-native-copy-file-bytes-empty-file
  "%copy-file-bytes handles empty source files."
  (uiop:with-temporary-file (:pathname src :type "bin")
    (uiop:with-temporary-file (:pathname dst :type "bin" :keep t)
      (with-open-file (out src :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8)))
      (cl-cc::%copy-file-bytes src dst)
      (assert-true (probe-file dst))
      (assert-= 0 (with-open-file (in dst :direction :input
                                          :element-type '(unsigned-byte 8))
                    (file-length in)))
      (ignore-errors (delete-file dst)))))

(deftest pipeline-native-copy-file-bytes-large-buffer
  "%copy-file-bytes handles files larger than the 4096-byte internal buffer."
  (uiop:with-temporary-file (:pathname src :type "bin")
    (uiop:with-temporary-file (:pathname dst :type "bin" :keep t)
      (let ((chunk "ABCDEFGHIJ"))
        (with-open-file (out src :direction :output
                                 :if-exists :supersede
                                 :element-type 'character)
          (loop repeat 450 do (write-string chunk out))))
      (let ((src-size (with-open-file (s src :element-type '(unsigned-byte 8))
                        (file-length s))))
        (assert-true (> src-size 4096))
        (cl-cc::%copy-file-bytes src dst)
        (let ((dst-size (with-open-file (s dst :element-type '(unsigned-byte 8))
                          (file-length s))))
          (assert-= src-size dst-size)))
      (ignore-errors (delete-file dst)))))

;;; ─── Typeclass macro registration ───────────────────────────────────────────

(deftest-each pipeline-native-typeclass-macros-registered-as-expanders
  "deftype-class and deftype-instance are each registered as invokable macro expanders."
  :cases (("deftype-class"    'cl-cc::deftype-class)
          ("deftype-instance" 'cl-cc::deftype-instance))
  (macro-name)
  (let ((expander (gethash macro-name
                            (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*))))
    (assert-true expander)
    (assert-true (or (functionp expander)
                     (eq (getf expander :kind) :macro-expander)
                     (eq (getf expander :kind) :register-macro-expander)))))

(deftest pipeline-native-typeclass-class-expander-builds-register-form
  "deftype-class expander produces a register-typeclass form backed by make-typeclass-def data."
  (let* ((expander (gethash 'cl-cc::deftype-class
                            (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*)))
         (expanded (cl-cc/expand::invoke-registered-expander
                    expander
                    '(deftype-class eq-like (a)
                       (equals (-> a a bool)))
                    nil)))
    (assert-eq 'progn (car expanded))
    (assert-true (search "REGISTER-TYPECLASS" (prin1-to-string expanded)))
    (assert-true (search "MAKE-TYPECLASS-DEF" (prin1-to-string expanded)))))

(deftest pipeline-native-typeclass-instance-expander-builds-register-form
  "deftype-instance expander produces register-typeclass-instance plus a dictionary defvar."
  (let* ((expander (gethash 'cl-cc::deftype-instance
                            (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*)))
         (expanded (cl-cc/expand::invoke-registered-expander
                    expander
                    '(deftype-instance eq-like integer
                       (equals (lambda (x y) (= x y))))
                    nil)))
    (assert-eq 'progn (car expanded))
    (assert-true (search "REGISTER-TYPECLASS-INSTANCE" (prin1-to-string expanded)))
    (assert-true (search "DEFVAR" (prin1-to-string expanded)))))

;;; ─── compile-file-to-native cache hit ──────────────────────────────────────

(deftest pipeline-native-compile-file-cache-hit-copies-artifact
  "compile-file-to-native reuses a cached native artifact when present."
  (uiop:with-temporary-file (:pathname input :type "php" :keep t)
    (uiop:with-temporary-file (:pathname output :type "bin" :keep t)
      (uiop:with-temporary-file (:pathname cache :type "bin" :keep t)
        (let ((copied nil)
              (chmod-called nil))
          (with-open-file (stream input :direction :output :if-exists :supersede)
            (write-line "<?php echo 1;" stream))
          (with-native-cache-stubs (:cache-path cache)
            (with-replaced-function (cl-cc::%copy-file-bytes
                                     (lambda (from to)
                                       (setf copied (list from to))
                                       to))
              (with-replaced-function (cl-cc::%run-short-native-command
                                       (lambda (&rest args)
                                         (declare (ignore args))
                                         (setf chmod-called t)
                                         nil))
                (assert-equal output
                              (cl-cc::compile-file-to-native input :output-file output))
                (assert-equal (list cache output) copied)
                (assert-true chmod-called))))
        (ignore-errors (delete-file cache)))
      (ignore-errors (delete-file output)))
    (ignore-errors (delete-file input)))))

(deftest pipeline-native-compile-file-cache-hit-skips-native-compilation
  "compile-file-to-native avoids native compilation work when the cache artifact exists."
  (uiop:with-temporary-file (:pathname input :type "php" :keep t)
    (uiop:with-temporary-file (:pathname output :type "bin" :keep t)
      (uiop:with-temporary-file (:pathname cache :type "bin" :keep t)
        (with-open-file (stream input :direction :output :if-exists :supersede)
          (write-line "<?php echo 1;" stream))
        (with-native-cache-stubs (:cache-path cache)
          (with-replaced-function (cl-cc::%compile-native-file-source
                                  (lambda (&rest args)
                                    (declare (ignore args))
                                    (error "cache hit should skip native compilation")))
            (assert-equal output
                          (cl-cc::compile-file-to-native input :output-file output))))
        (ignore-errors (delete-file cache)))
      (ignore-errors (delete-file output)))
    (ignore-errors (delete-file input))))

(deftest pipeline-native-compile-file-cache-key-receives-option-plist
  "compile-file-to-native passes native compile options into the cache key."
  (uiop:with-temporary-file (:pathname input :type "php" :keep t)
    (uiop:with-temporary-file (:pathname output :type "bin" :keep t)
      (uiop:with-temporary-file (:pathname cache :type "bin" :keep t)
        (let ((captured-args nil))
          (with-open-file (stream input :direction :output :if-exists :supersede)
            (write-line "<?php echo 1;" stream))
          (with-replaced-function (cl-cc::%compile-cache-key
                                   (lambda (&rest args)
                                     (setf captured-args args)
                                     "cache-key"))
            (with-replaced-function (cl-cc::%compile-cache-path
                                     (lambda (&rest args)
                                       (declare (ignore args))
                                       cache))
              (with-replaced-function (cl-cc::%copy-file-bytes
                                       (lambda (from to)
                                         (declare (ignore from))
                                         to))
                (assert-equal output
                              (cl-cc::compile-file-to-native
                               input :output-file output
                               :speed 3
                               :inline-threshold-scale 2
                               :pass-pipeline '(:fold :dce))))))
          (assert-= 4 (length captured-args))
          (assert-equal '(:fold :dce) (getf (fourth captured-args) :pass-pipeline))
          (assert-= 2 (getf (fourth captured-args) :inline-threshold-scale))
          (assert-= 3 (getf (fourth captured-args) :speed)))
        (ignore-errors (delete-file cache)))
      (ignore-errors (delete-file output)))
    (ignore-errors (delete-file input))))

;;; ─── CPS-safe AST allowlist ─────────────────────────────────────────────────

(deftest pipeline-native-cps-safe-ast-p-rejects-io-and-mv-forms-until-native-cps-lowering-exists
  "%cps-native-compile-safe-ast-p rejects call and multiple-value forms while native CPS lowering is disabled."
  (let ((safe-ast (cl-cc:make-ast-call :func 'f :args (list (cl-cc:make-ast-int :value 1))))
        (mv-ast (cl-cc::make-ast-multiple-value-prog1
                 :first (cl-cc:make-ast-int :value 1)
                 :forms (list (cl-cc:make-ast-int :value 2))))
        (unsafe-ast (cl-cc/ast:make-ast-make-instance
                     :class (cl-cc:make-ast-quote :value 'point)
                     :initargs nil)))
    (assert-false (cl-cc::%cps-native-compile-safe-ast-p safe-ast))
    (assert-false (cl-cc::%cps-native-compile-safe-ast-p mv-ast))
    (assert-false (cl-cc::%cps-native-compile-safe-ast-p unsafe-ast))))

;;; ─── AutoFDO / sample-based PGO ingestion (pipeline-autofdo.lisp) ────────────
;;;
;;; These tests cover the FR-525 AutoFDO layer: little-endian byte readers, the
;;; text/perf-script line parser, perf-map lookup, hot/cold layout, the
;;; conservative binary perf.data parser, and the instruction-rewriting passes.
;;; A non-existent perf-map path is passed explicitly to keep results
;;; deterministic (the default probes a live /tmp/perf-<pid>.map).

(defun %autofdo-nonexistent-map-path ()
  "A perf-map pathname guaranteed not to exist, for deterministic ingestion."
  (format nil "/tmp/cl-cc-autofdo-absent-~D.map" (random 1000000000)))

;;; ─── little-endian byte readers ─────────────────────────────────────────────

(deftest autofdo-uNNle-read-values-in-bounds
  "u16/u32/u64 little-endian readers assemble bytes least-significant-first."
  (let ((bytes (make-array 8 :element-type '(unsigned-byte 8)
                             :initial-contents '(1 2 3 4 5 6 7 8))))
    (assert-= (+ 1 (ash 2 8)) (cl-cc/pipeline::%autofdo-u16le bytes 0))
    (assert-= (+ 1 (ash 2 8) (ash 3 16) (ash 4 24))
              (cl-cc/pipeline::%autofdo-u32le bytes 0))
    (assert-= (loop for i below 8 sum (ash (+ i 1) (* i 8)))
              (cl-cc/pipeline::%autofdo-u64le bytes 0))))

(deftest autofdo-uNNle-return-nil-past-end
  "The readers return NIL rather than erroring when the window runs off the end."
  (let ((bytes (make-array 8 :element-type '(unsigned-byte 8)
                             :initial-contents '(1 2 3 4 5 6 7 8))))
    (assert-null (cl-cc/pipeline::%autofdo-u16le bytes 7))
    (assert-null (cl-cc/pipeline::%autofdo-u32le bytes 6))
    (assert-null (cl-cc/pipeline::%autofdo-u64le bytes 1))))

;;; ─── binary-vs-text detection ───────────────────────────────────────────────

(deftest-each autofdo-binary-profile-p-cases
  "%autofdo-binary-profile-p recognizes perf.data magic and non-text bytes."
  :cases (("nil"          nil                                   nil)
          ("perfile2"     (map 'vector #'char-code "PERFILE2..") t)
          ("plain-text"   (map 'vector #'char-code "0x10 5")    nil)
          ("embedded-nul" #(104 101 0 108)                       t))
  (input expected)
  (let ((bytes (and input (coerce input '(vector (unsigned-byte 8))))))
    (if expected
        (assert-true (cl-cc/pipeline::%autofdo-binary-profile-p bytes))
        (assert-null (cl-cc/pipeline::%autofdo-binary-profile-p bytes)))))

;;; ─── IP token parsing ───────────────────────────────────────────────────────

(deftest-each autofdo-parse-ip-cases
  "%autofdo-parse-ip understands 0x-prefixed, bare-hex, and mixed tokens."
  :cases (("0x-prefixed" "0x1000" 4096)
          ("bare-hex"    "dead"   57005)
          ("with-colon"  ":0x20:" 32))
  (token expected)
  (assert-= expected (cl-cc/pipeline::%autofdo-parse-ip token)))

;;; ─── text / perf-script line parsing ────────────────────────────────────────

(deftest autofdo-sample-from-line-autofdo-row
  "An AutoFDO-style \"IP COUNT\" row yields (ip count); the IP token is read as hex."
  (assert-equal (list 4096 5) (cl-cc/pipeline::%autofdo-sample-from-line "0x1000 5"))
  (assert-equal (list 4096 3) (cl-cc/pipeline::%autofdo-sample-from-line "1000 3")))

(deftest autofdo-sample-from-line-perf-script-row
  "A perf-script row with one hex token defaults the count to 1."
  (assert-equal (list 57005 1)
                (cl-cc/pipeline::%autofdo-sample-from-line "cpu-clock: 0xdead foo")))

(deftest-each autofdo-sample-from-line-no-sample-cases
  "Lines without any usable IP token yield NIL."
  :cases (("blank"    "   ")
          ("no-hex"   "hello world"))
  (line)
  (assert-null (cl-cc/pipeline::%autofdo-sample-from-line line)))

;;; ─── perf-map file reading + IP→function mapping ────────────────────────────

(deftest autofdo-read-perf-map-parses-valid-rows-and-skips-junk
  "read-perf-map keeps HEX-ADDR HEX-SIZE NAME rows (sorted) and drops malformed ones."
  (uiop:with-temporary-file (:pathname map :type "map")
    (with-open-file (out map :direction :output :if-exists :supersede)
      (write-line "2000 100 later" out)
      (write-line "garbage-line" out)
      (write-line "1000 100 earlier" out))
    (let ((entries (cl-cc/pipeline::read-perf-map map)))
      (assert-= 2 (length entries))
      (assert-equal (list #x1000 #x1100 "earlier") (first entries))
      (assert-equal (list #x2000 #x2100 "later") (second entries)))))

(deftest-each autofdo-map-ip-to-function-cases
  "autofdo-map-ip-to-function returns the symbol for an in-range IP, NIL otherwise."
  :cases (("start-inclusive" #x1000 "f")
          ("mid-range"       #x1080 "f")
          ("end-exclusive"   #x1100 nil)
          ("below-range"     #x0500 nil))
  (ip expected)
  (let ((perf-map (list (list #x1000 #x1100 "f"))))
    (assert-equal expected (cl-cc/pipeline::autofdo-map-ip-to-function ip perf-map))))

;;; ─── hot/cold layout decisions ──────────────────────────────────────────────

(deftest autofdo-hot-cold-layout-splits-by-percentile
  "The hottest function stays :hot; a negligible tail function is marked :cold."
  (let ((rows (cl-cc/pipeline::autofdo-hot-cold-layout-decisions
               (list (cons "hot" 100) (cons "cold" 1)))))
    (assert-= 2 (length rows))
    (assert-eq :hot  (getf (first rows) :layout))
    (assert-eq :cold (getf (second rows) :layout))))

(deftest-each autofdo-hot-cold-layout-edge-cases
  "Empty input yields no decisions; a lone function is always :hot."
  :cases (("empty"  nil                     nil)
          ("single" (list (cons "solo" 7))  :hot))
  (rows expected-layout)
  (let ((decisions (cl-cc/pipeline::autofdo-hot-cold-layout-decisions rows)))
    (if expected-layout
        (assert-eq expected-layout (getf (first decisions) :layout))
        (assert-null decisions))))

;;; ─── text profile ingestion end to end ──────────────────────────────────────

(deftest autofdo-read-text-profile-aggregates-hotness-through-perf-map
  "read-autofdo-profile maps sample IPs through the perf-map and sums per-function counts."
  (uiop:with-temporary-file (:pathname map :type "map")
    (uiop:with-temporary-file (:pathname prof :type "txt")
      (with-open-file (out map :direction :output :if-exists :supersede)
        (write-line "1000 1000 myfunc" out))
      (with-open-file (out prof :direction :output :if-exists :supersede)
        (write-line "0x1000 5" out)
        (write-line "0x1500 3" out))
      (let* ((profile (cl-cc/pipeline::read-autofdo-profile prof :perf-map-path map))
             (hotness (cl-cc/pipeline::autofdo-profile-function-hotness profile)))
        (assert-equal (list (cons "myfunc" 8)) hotness)
        (assert-= 2 (length (cl-cc/pipeline::autofdo-profile-raw-samples profile)))
        (assert-true (cl-cc/pipeline::autofdo-profile-layout-decisions profile))))))

(deftest autofdo-read-text-profile-unmapped-ips-produce-empty-hotness
  "Samples whose IPs miss every perf-map entry contribute no function hotness."
  (uiop:with-temporary-file (:pathname prof :type "txt")
    (with-open-file (out prof :direction :output :if-exists :supersede)
      (write-line "0x9000 5" out))
    (let ((profile (cl-cc/pipeline::read-autofdo-profile
                    prof :perf-map-path (%autofdo-nonexistent-map-path))))
      (assert-null (cl-cc/pipeline::autofdo-profile-function-hotness profile))
      (assert-= 1 (length (cl-cc/pipeline::autofdo-profile-raw-samples profile))))))

;;; ─── binary perf.data parsing ───────────────────────────────────────────────

(deftest autofdo-read-perf-data-binary-consumes-mmap-and-sample-records
  "read-perf-data-binary symbolizes a SAMPLE IP through an in-file MMAP record."
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; PERF_RECORD_MMAP at offset 0: type=1, size=48, addr=0x1000, len=0x1000.
    (setf (aref buffer 0) 1 (aref buffer 6) 48)
    (setf (aref buffer 16) #x00 (aref buffer 17) #x10)   ; addr = 0x1000
    (setf (aref buffer 24) #x00 (aref buffer 25) #x10)   ; len  = 0x1000
    (loop for ch across "myfunc" for i from 40
          do (setf (aref buffer i) (char-code ch)))       ; filename → "myfunc"
    ;; PERF_RECORD_SAMPLE at offset 48: type=9, size=16, ip=0x1500.
    (setf (aref buffer 48) 9 (aref buffer 54) 16)
    (setf (aref buffer 56) #x00 (aref buffer 57) #x15)   ; ip = 0x1500
    (uiop:with-temporary-file (:pathname prof :type "data")
      (with-open-file (out prof :direction :output :if-exists :supersede
                               :element-type '(unsigned-byte 8))
        (write-sequence buffer out))
      (let ((profile (cl-cc/pipeline::read-perf-data-binary
                      prof :perf-map-path (%autofdo-nonexistent-map-path))))
        (assert-equal (list (cons "myfunc" 1))
                      (cl-cc/pipeline::autofdo-profile-function-hotness profile))
        (assert-= 1 (length (cl-cc/pipeline::autofdo-profile-raw-samples profile)))))))

(deftest autofdo-read-profile-routes-binary-data-to-binary-parser
  "read-autofdo-profile detects perf.data-shaped bytes and uses the binary parser."
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref buffer 0) 9 (aref buffer 6) 16)
    (setf (aref buffer 8) #x00 (aref buffer 9) #x15)
    (uiop:with-temporary-file (:pathname prof :type "data")
      (with-open-file (out prof :direction :output :if-exists :supersede
                               :element-type '(unsigned-byte 8))
        (write-sequence buffer out))
      ;; The embedded NUL bytes make %autofdo-binary-profile-p true.
      (let ((profile (cl-cc/pipeline::read-autofdo-profile
                      prof :perf-map-path (%autofdo-nonexistent-map-path))))
        (assert-true (cl-cc/pipeline::autofdo-profile-p profile))))))

;;; ─── plist conversion + lazy loading ────────────────────────────────────────

(deftest autofdo-load-profile-data-tags-plist-format
  "load-autofdo-profile-data returns a :cl-cc-autofdo-v1 plist carrying hotness rows."
  (uiop:with-temporary-file (:pathname map :type "map")
    (uiop:with-temporary-file (:pathname prof :type "txt")
      (with-open-file (out map :direction :output :if-exists :supersede)
        (write-line "1000 1000 g" out))
      (with-open-file (out prof :direction :output :if-exists :supersede)
        (write-line "0x1000 4" out))
      (let ((plist (cl-cc/pipeline::load-autofdo-profile-data prof :perf-map-path map)))
        (assert-eq :cl-cc-autofdo-v1 (getf plist :format))
        (assert-equal (list (cons "g" 4)) (getf plist :function-hotness))))))

(deftest-each autofdo-maybe-load-profile-data-cases
  "maybe-load-autofdo-profile-data loads only when given a path; other data passes through."
  :cases (("already-plist" (list :format :cl-cc-autofdo-v1))
          ("nil"           nil))
  (data)
  (assert-eq data (cl-cc/pipeline::maybe-load-autofdo-profile-data data)))

;;; ─── layout-decision instruction rewriting ──────────────────────────────────

(deftest autofdo-apply-layout-decisions-nil-is-passthrough
  "With no layout decisions the instruction list is returned unchanged."
  (let ((insts (list (cl-cc:make-vm-const :dst :r0 :value 1))))
    (assert-eq insts (cl-cc/pipeline::autofdo-apply-layout-decisions insts nil))))

(deftest autofdo-apply-layout-decisions-moves-cold-block-after-hot
  "A block whose label is marked :cold is relocated after hot/unknown blocks."
  (let* ((l-cold (cl-cc:make-vm-label :name :cold))
         (c-cold (cl-cc:make-vm-const :dst :r0 :value 0))
         (l-hot  (cl-cc:make-vm-label :name :hot))
         (c-hot  (cl-cc:make-vm-const :dst :r1 :value 1))
         (insts  (list l-cold c-cold l-hot c-hot))
         (profile-data (list :layout-decisions
                             (list (list :function "COLD" :layout :cold))))
         (result (cl-cc/pipeline::autofdo-apply-layout-decisions insts profile-data)))
    ;; Hot block now leads; the cold block is appended at the tail.
    (assert-eq l-hot (first result))
    (assert-eq l-cold (third result))))

;;; ─── branch-probability instruction rewriting ───────────────────────────────

(deftest autofdo-apply-branch-probabilities-nil-is-passthrough
  "Without branch samples the pass does not transform instructions."
  (let ((insts (list (cl-cc:make-vm-jump-zero :reg :r0 :label :target))))
    (assert-eq insts (cl-cc/pipeline::autofdo-apply-branch-probabilities insts nil))))

(deftest autofdo-apply-branch-probabilities-weights-jump-zero
  "A jump-zero at a sampled PC becomes a branch-weighted jump carrying the taken bias."
  (let* ((jz (cl-cc:make-vm-jump-zero :reg :r0 :label :target))
         (profile-data (list :branch-probabilities (list (cons 0 0.9))))
         (result (cl-cc/pipeline::autofdo-apply-branch-probabilities
                  (list jz) profile-data))
         (weighted (first result)))
    (assert-true (typep weighted 'cl-cc/optimize::vm-branch-weighted-jump-zero))
    (assert-eq :likely-taken
               (cl-cc/optimize::vm-branch-weighted-jump-zero-branch-weight weighted))))
