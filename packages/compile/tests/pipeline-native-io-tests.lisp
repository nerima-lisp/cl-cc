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
