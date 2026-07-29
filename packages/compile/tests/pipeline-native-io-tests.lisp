;;;; tests/unit/compile/pipeline-native-io-tests.lisp — Pipeline Native I/O + Integration
;;;;
;;;; Tests for %copy-file-bytes, compile-file-to-native cache hits, typeclass
;;;; macro registration, and %cps-native-compile-safe-ast-p.
;;;; Suite: pipeline-native-suite (defined in pipeline-native-tests.lisp).

(in-package :cl-cc/test)

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

(it-sequential "pipeline-native-copy-file-bytes-returns-dst-and-creates-file"
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
        (expect (pathnamep result) :to-be-truthy)
        (expect (namestring result) :to-equal (namestring dst))
        (expect (probe-file dst) :to-be-truthy)
        (ignore-errors (delete-file dst))))))

(it-sequential "pipeline-native-copy-file-bytes-same-contents"
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
          (expect (coerce read-back 'list) :to-equal (coerce data 'list)))
        (ignore-errors (delete-file dst))))))

(it-sequential "pipeline-native-copy-file-bytes-empty-file"
  (uiop:with-temporary-file (:pathname src :type "bin")
    (uiop:with-temporary-file (:pathname dst :type "bin" :keep t)
      (with-open-file (out src :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8)))
      (cl-cc::%copy-file-bytes src dst)
      (expect (probe-file dst) :to-be-truthy)
      (expect (= 0 (with-open-file (in dst :direction :input
                                          :element-type '(unsigned-byte 8))
                    (file-length in))) :to-be-truthy)
      (ignore-errors (delete-file dst)))))

(it-sequential "pipeline-native-copy-file-bytes-large-buffer"
  (uiop:with-temporary-file (:pathname src :type "bin")
    (uiop:with-temporary-file (:pathname dst :type "bin" :keep t)
      (let ((chunk "ABCDEFGHIJ"))
        (with-open-file (out src :direction :output
                                 :if-exists :supersede
                                 :element-type 'character)
          (loop repeat 450 do (write-string chunk out))))
      (let ((src-size (with-open-file (s src :element-type '(unsigned-byte 8))
                        (file-length s))))
        (expect (> src-size 4096) :to-be-truthy)
        (cl-cc::%copy-file-bytes src dst)
        (let ((dst-size (with-open-file (s dst :element-type '(unsigned-byte 8))
                          (file-length s))))
          (expect (= src-size dst-size) :to-be-truthy)))
      (ignore-errors (delete-file dst)))))

;;; ─── Typeclass macro registration ───────────────────────────────────────────

(it-sequential "pipeline-native-typeclass-macros-registered-as-expanders deftype-class"
  (destructuring-bind (macro-name) (list 'cl-cc::deftype-class)
    (let ((expander (gethash macro-name
                            (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*))))
    (expect expander :to-be-truthy)
    (expect (or (functionp expander)
                     (eq (getf expander :kind) :macro-expander)
                     (eq (getf expander :kind) :register-macro-expander)) :to-be-truthy))))

(it-sequential "pipeline-native-typeclass-macros-registered-as-expanders deftype-instance"
  (destructuring-bind (macro-name) (list 'cl-cc::deftype-instance)
    (let ((expander (gethash macro-name
                            (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*))))
    (expect expander :to-be-truthy)
    (expect (or (functionp expander)
                     (eq (getf expander :kind) :macro-expander)
                     (eq (getf expander :kind) :register-macro-expander)) :to-be-truthy))))

(it-sequential "pipeline-native-typeclass-class-expander-builds-register-form"
  (let* ((expander (gethash 'cl-cc::deftype-class
                            (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*)))
         (expanded (cl-cc/expand::invoke-registered-expander
                    expander
                    '(deftype-class eq-like (a)
                       (equals (-> a a bool)))
                    nil)))
    (expect (car expanded) :to-be 'progn)
    (expect (search "REGISTER-TYPECLASS" (prin1-to-string expanded)) :to-be-truthy)
    (expect (search "MAKE-TYPECLASS-DEF" (prin1-to-string expanded)) :to-be-truthy)))

(it-sequential "pipeline-native-typeclass-instance-expander-builds-register-form"
  (let* ((expander (gethash 'cl-cc::deftype-instance
                            (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*)))
         (expanded (cl-cc/expand::invoke-registered-expander
                    expander
                    '(deftype-instance eq-like integer
                       (equals (lambda (x y) (= x y))))
                    nil)))
    (expect (car expanded) :to-be 'progn)
    (expect (search "REGISTER-TYPECLASS-INSTANCE" (prin1-to-string expanded)) :to-be-truthy)
    (expect (search "DEFVAR" (prin1-to-string expanded)) :to-be-truthy)))

;;; ─── compile-file-to-native cache hit ──────────────────────────────────────

(it-sequential "pipeline-native-compile-file-cache-hit-copies-artifact"
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
                (expect (cl-cc::compile-file-to-native input :output-file output) :to-equal output)
                (expect copied :to-equal (list cache output))
                (expect chmod-called :to-be-truthy))))
        (ignore-errors (delete-file cache)))
      (ignore-errors (delete-file output)))
    (ignore-errors (delete-file input)))))

(it-sequential "pipeline-native-compile-file-cache-hit-skips-native-compilation"
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
            (expect (cl-cc::compile-file-to-native input :output-file output) :to-equal output)))
        (ignore-errors (delete-file cache)))
      (ignore-errors (delete-file output)))
    (ignore-errors (delete-file input))))

(it-sequential "pipeline-native-compile-file-cache-key-receives-option-plist"
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
                (expect (cl-cc::compile-file-to-native
                               input :output-file output
                               :speed 3
                               :inline-threshold-scale 2
                               :pass-pipeline '(:fold :dce)) :to-equal output))))
          (expect (= 4 (length captured-args)) :to-be-truthy)
          (expect (getf (fourth captured-args) :pass-pipeline) :to-equal '(:fold :dce))
          (expect (= 2 (getf (fourth captured-args) :inline-threshold-scale)) :to-be-truthy)
          (expect (= 3 (getf (fourth captured-args) :speed)) :to-be-truthy))
        (ignore-errors (delete-file cache)))
      (ignore-errors (delete-file output)))
    (ignore-errors (delete-file input))))

;;; ─── CPS-safe AST allowlist ─────────────────────────────────────────────────

(it-sequential "pipeline-native-cps-safe-ast-p-rejects-io-and-mv-forms-until-native-cps-lowering-exists"
  (let ((safe-ast (cl-cc:make-ast-call :func 'f :args (list (cl-cc:make-ast-int :value 1))))
        (mv-ast (cl-cc::make-ast-multiple-value-prog1
                 :first (cl-cc:make-ast-int :value 1)
                 :forms (list (cl-cc:make-ast-int :value 2))))
        (unsafe-ast (cl-cc/ast:make-ast-make-instance
                     :class (cl-cc:make-ast-quote :value 'point)
                     :initargs nil)))
    (expect (cl-cc::%cps-native-compile-safe-ast-p safe-ast) :to-be-falsy)
    (expect (cl-cc::%cps-native-compile-safe-ast-p mv-ast) :to-be-falsy)
    (expect (cl-cc::%cps-native-compile-safe-ast-p unsafe-ast) :to-be-falsy)))

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

(it-sequential "autofdo-unnle-read-values-in-bounds"
  (let ((bytes (make-array 8 :element-type '(unsigned-byte 8)
                             :initial-contents '(1 2 3 4 5 6 7 8))))
    (expect (= (+ 1 (ash 2 8)) (cl-cc/pipeline::%autofdo-u16le bytes 0)) :to-be-truthy)
    (expect (= (+ 1 (ash 2 8) (ash 3 16) (ash 4 24)) (cl-cc/pipeline::%autofdo-u32le bytes 0)) :to-be-truthy)
    (expect (= (loop for i below 8 sum (ash (+ i 1) (* i 8))) (cl-cc/pipeline::%autofdo-u64le bytes 0)) :to-be-truthy)))

(it-sequential "autofdo-unnle-return-nil-past-end"
  (let ((bytes (make-array 8 :element-type '(unsigned-byte 8)
                             :initial-contents '(1 2 3 4 5 6 7 8))))
    (expect (cl-cc/pipeline::%autofdo-u16le bytes 7) :to-be-null)
    (expect (cl-cc/pipeline::%autofdo-u32le bytes 6) :to-be-null)
    (expect (cl-cc/pipeline::%autofdo-u64le bytes 1) :to-be-null)))

;;; ─── binary-vs-text detection ───────────────────────────────────────────────

(it-sequential "autofdo-binary-profile-p-cases nil"
  (destructuring-bind (input expected) (list nil nil)
    (let ((bytes (and input (coerce input '(vector (unsigned-byte 8))))))
    (if expected
        (expect (cl-cc/pipeline::%autofdo-binary-profile-p bytes) :to-be-truthy)
        (expect (cl-cc/pipeline::%autofdo-binary-profile-p bytes) :to-be-null)))))

(it-sequential "autofdo-binary-profile-p-cases perfile2"
  (destructuring-bind (input expected) (list (map 'vector #'char-code "PERFILE2..") t)
    (let ((bytes (and input (coerce input '(vector (unsigned-byte 8))))))
    (if expected
        (expect (cl-cc/pipeline::%autofdo-binary-profile-p bytes) :to-be-truthy)
        (expect (cl-cc/pipeline::%autofdo-binary-profile-p bytes) :to-be-null)))))

(it-sequential "autofdo-binary-profile-p-cases plain-text"
  (destructuring-bind (input expected) (list (map 'vector #'char-code "0x10 5") nil)
    (let ((bytes (and input (coerce input '(vector (unsigned-byte 8))))))
    (if expected
        (expect (cl-cc/pipeline::%autofdo-binary-profile-p bytes) :to-be-truthy)
        (expect (cl-cc/pipeline::%autofdo-binary-profile-p bytes) :to-be-null)))))

(it-sequential "autofdo-binary-profile-p-cases embedded-nul"
  (destructuring-bind (input expected) (list #(104 101 0 108) t)
    (let ((bytes (and input (coerce input '(vector (unsigned-byte 8))))))
    (if expected
        (expect (cl-cc/pipeline::%autofdo-binary-profile-p bytes) :to-be-truthy)
        (expect (cl-cc/pipeline::%autofdo-binary-profile-p bytes) :to-be-null)))))

;;; ─── IP token parsing ───────────────────────────────────────────────────────

(it-sequential "autofdo-parse-ip-cases 0x-prefixed"
  (destructuring-bind (token expected) (list "0x1000" 4096)
    (expect (= expected (cl-cc/pipeline::%autofdo-parse-ip token)) :to-be-truthy)))

(it-sequential "autofdo-parse-ip-cases bare-hex"
  (destructuring-bind (token expected) (list "dead" 57005)
    (expect (= expected (cl-cc/pipeline::%autofdo-parse-ip token)) :to-be-truthy)))

(it-sequential "autofdo-parse-ip-cases with-colon"
  (destructuring-bind (token expected) (list ":0x20:" 32)
    (expect (= expected (cl-cc/pipeline::%autofdo-parse-ip token)) :to-be-truthy)))

;;; ─── text / perf-script line parsing ────────────────────────────────────────

(it-sequential "autofdo-sample-from-line-autofdo-row"
  (expect (cl-cc/pipeline::%autofdo-sample-from-line "0x1000 5") :to-equal (list 4096 5))
  (expect (cl-cc/pipeline::%autofdo-sample-from-line "1000 3") :to-equal (list 4096 3)))

(it-sequential "autofdo-sample-from-line-perf-script-row"
  (expect (cl-cc/pipeline::%autofdo-sample-from-line "cpu-clock: 0xdead foo") :to-equal (list 57005 1)))

(it-sequential "autofdo-sample-from-line-no-sample-cases blank"
  (destructuring-bind (line) (list "   ")
    (expect (cl-cc/pipeline::%autofdo-sample-from-line line) :to-be-null)))

(it-sequential "autofdo-sample-from-line-no-sample-cases no-hex"
  (destructuring-bind (line) (list "hello world")
    (expect (cl-cc/pipeline::%autofdo-sample-from-line line) :to-be-null)))

;;; ─── perf-map file reading + IP→function mapping ────────────────────────────

(it-sequential "autofdo-read-perf-map-parses-valid-rows-and-skips-junk"
  (uiop:with-temporary-file (:pathname map :type "map")
    (with-open-file (out map :direction :output :if-exists :supersede)
      (write-line "2000 100 later" out)
      (write-line "garbage-line" out)
      (write-line "1000 100 earlier" out))
    (let ((entries (cl-cc/pipeline::read-perf-map map)))
      (expect (= 2 (length entries)) :to-be-truthy)
      (expect (first entries) :to-equal (list #x1000 #x1100 "earlier"))
      (expect (second entries) :to-equal (list #x2000 #x2100 "later")))))

(it-sequential "autofdo-map-ip-to-function-cases start-inclusive"
  (destructuring-bind (ip expected) (list #x1000 "f")
    (let ((perf-map (list (list #x1000 #x1100 "f"))))
    (expect (cl-cc/pipeline::autofdo-map-ip-to-function ip perf-map) :to-equal expected))))

(it-sequential "autofdo-map-ip-to-function-cases mid-range"
  (destructuring-bind (ip expected) (list #x1080 "f")
    (let ((perf-map (list (list #x1000 #x1100 "f"))))
    (expect (cl-cc/pipeline::autofdo-map-ip-to-function ip perf-map) :to-equal expected))))

(it-sequential "autofdo-map-ip-to-function-cases end-exclusive"
  (destructuring-bind (ip expected) (list #x1100 nil)
    (let ((perf-map (list (list #x1000 #x1100 "f"))))
    (expect (cl-cc/pipeline::autofdo-map-ip-to-function ip perf-map) :to-equal expected))))

(it-sequential "autofdo-map-ip-to-function-cases below-range"
  (destructuring-bind (ip expected) (list #x0500 nil)
    (let ((perf-map (list (list #x1000 #x1100 "f"))))
    (expect (cl-cc/pipeline::autofdo-map-ip-to-function ip perf-map) :to-equal expected))))

;;; ─── hot/cold layout decisions ──────────────────────────────────────────────

(it-sequential "autofdo-hot-cold-layout-splits-by-percentile"
  (let ((rows (cl-cc/pipeline::autofdo-hot-cold-layout-decisions
               (list (cons "hot" 100) (cons "cold" 1)))))
    (expect (= 2 (length rows)) :to-be-truthy)
    (expect (getf (first rows) :layout) :to-be :hot)
    (expect (getf (second rows) :layout) :to-be :cold)))

(it-sequential "autofdo-hot-cold-layout-edge-cases empty"
  (destructuring-bind (rows expected-layout) (list nil nil)
    (let ((decisions (cl-cc/pipeline::autofdo-hot-cold-layout-decisions rows)))
    (if expected-layout
        (expect (getf (first decisions) :layout) :to-be expected-layout)
        (expect decisions :to-be-null)))))

(it-sequential "autofdo-hot-cold-layout-edge-cases single"
  (destructuring-bind (rows expected-layout) (list (list (cons "solo" 7)) :hot)
    (let ((decisions (cl-cc/pipeline::autofdo-hot-cold-layout-decisions rows)))
    (if expected-layout
        (expect (getf (first decisions) :layout) :to-be expected-layout)
        (expect decisions :to-be-null)))))

;;; ─── text profile ingestion end to end ──────────────────────────────────────

(it-sequential "autofdo-read-text-profile-aggregates-hotness-through-perf-map"
  (uiop:with-temporary-file (:pathname map :type "map")
    (uiop:with-temporary-file (:pathname prof :type "txt")
      (with-open-file (out map :direction :output :if-exists :supersede)
        (write-line "1000 1000 myfunc" out))
      (with-open-file (out prof :direction :output :if-exists :supersede)
        (write-line "0x1000 5" out)
        (write-line "0x1500 3" out))
      (let* ((profile (cl-cc/pipeline::read-autofdo-profile prof :perf-map-path map))
             (hotness (cl-cc/pipeline::autofdo-profile-function-hotness profile)))
        (expect hotness :to-equal (list (cons "myfunc" 8)))
        (expect (= 2 (length (cl-cc/pipeline::autofdo-profile-raw-samples profile))) :to-be-truthy)
        (expect (cl-cc/pipeline::autofdo-profile-layout-decisions profile) :to-be-truthy)))))

(it-sequential "autofdo-read-text-profile-unmapped-ips-produce-empty-hotness"
  (uiop:with-temporary-file (:pathname prof :type "txt")
    (with-open-file (out prof :direction :output :if-exists :supersede)
      (write-line "0x9000 5" out))
    (let ((profile (cl-cc/pipeline::read-autofdo-profile
                    prof :perf-map-path (%autofdo-nonexistent-map-path))))
      (expect (cl-cc/pipeline::autofdo-profile-function-hotness profile) :to-be-null)
      (expect (= 1 (length (cl-cc/pipeline::autofdo-profile-raw-samples profile))) :to-be-truthy))))

;;; ─── binary perf.data parsing ───────────────────────────────────────────────

(it-sequential "autofdo-read-perf-data-binary-consumes-mmap-and-sample-records"
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
        (expect (cl-cc/pipeline::autofdo-profile-function-hotness profile) :to-equal (list (cons "myfunc" 1)))
        (expect (= 1 (length (cl-cc/pipeline::autofdo-profile-raw-samples profile))) :to-be-truthy)))))

(it-sequential "autofdo-read-profile-routes-binary-data-to-binary-parser"
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
        (expect (cl-cc/pipeline::autofdo-profile-p profile) :to-be-truthy)))))

;;; ─── plist conversion + lazy loading ────────────────────────────────────────

(it-sequential "autofdo-load-profile-data-tags-plist-format"
  (uiop:with-temporary-file (:pathname map :type "map")
    (uiop:with-temporary-file (:pathname prof :type "txt")
      (with-open-file (out map :direction :output :if-exists :supersede)
        (write-line "1000 1000 g" out))
      (with-open-file (out prof :direction :output :if-exists :supersede)
        (write-line "0x1000 4" out))
      (let ((plist (cl-cc/pipeline::load-autofdo-profile-data prof :perf-map-path map)))
        (expect (getf plist :format) :to-be :cl-cc-autofdo-v1)
        (expect (getf plist :function-hotness) :to-equal (list (cons "g" 4)))))))

(it-sequential "autofdo-maybe-load-profile-data-cases already-plist"
  (destructuring-bind (data) (list (list :format :cl-cc-autofdo-v1))
    (expect (cl-cc/pipeline::maybe-load-autofdo-profile-data data) :to-be data)))

(it-sequential "autofdo-maybe-load-profile-data-cases nil"
  (destructuring-bind (data) (list nil)
    (expect (cl-cc/pipeline::maybe-load-autofdo-profile-data data) :to-be data)))

;;; ─── layout-decision instruction rewriting ──────────────────────────────────

(it-sequential "autofdo-apply-layout-decisions-nil-is-passthrough"
  (let ((insts (list (cl-cc:make-vm-const :dst :r0 :value 1))))
    (expect (cl-cc/pipeline::autofdo-apply-layout-decisions insts nil) :to-be insts)))

(it-sequential "autofdo-apply-layout-decisions-moves-cold-block-after-hot"
  (let* ((l-cold (cl-cc:make-vm-label :name :cold))
         (c-cold (cl-cc:make-vm-const :dst :r0 :value 0))
         (l-hot  (cl-cc:make-vm-label :name :hot))
         (c-hot  (cl-cc:make-vm-const :dst :r1 :value 1))
         (insts  (list l-cold c-cold l-hot c-hot))
         (profile-data (list :layout-decisions
                             (list (list :function "COLD" :layout :cold))))
         (result (cl-cc/pipeline::autofdo-apply-layout-decisions insts profile-data)))
    ;; Hot block now leads; the cold block is appended at the tail.
    (expect (first result) :to-be l-hot)
    (expect (third result) :to-be l-cold)))

;;; ─── branch-probability instruction rewriting ───────────────────────────────

(it-sequential "autofdo-apply-branch-probabilities-nil-is-passthrough"
  (let ((insts (list (cl-cc:make-vm-jump-zero :reg :r0 :label :target))))
    (expect (cl-cc/pipeline::autofdo-apply-branch-probabilities insts nil) :to-be insts)))

(it-sequential "autofdo-apply-branch-probabilities-weights-jump-zero"
  (let* ((jz (cl-cc:make-vm-jump-zero :reg :r0 :label :target))
         (profile-data (list :branch-probabilities (list (cons 0 0.9))))
         (result (cl-cc/pipeline::autofdo-apply-branch-probabilities
                  (list jz) profile-data))
         (weighted (first result)))
    (expect (typep weighted 'cl-cc/optimize::vm-branch-weighted-jump-zero) :to-be-truthy)
    (expect (cl-cc/optimize::vm-branch-weighted-jump-zero-branch-weight weighted) :to-be :likely-taken)))
