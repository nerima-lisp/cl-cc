;;;; tests/unit/compile/pipeline-native-tests.lisp — Pipeline Native tests
;;;;
;;;; Tests for pipeline-native.lisp: *compile-cache-root*, %compile-cache-key,
;;;; %compile-cache-path, and %make-native-opts.
;;;; File I/O + typeclass + integration → pipeline-native-io-tests.lisp.

(in-package :cl-cc/test)



;;; ─── %make-native-opts ──────────────────────────────────────────────────────

(it-sequential "pipeline-native-make-opts-defaults"
  (let ((opts (cl-cc::%make-native-opts)))
    (expect (listp opts) :to-be-truthy)
    (expect (getf opts :pass-pipeline) :to-be-null)
    (expect (= 1 (getf opts :inline-threshold-scale)) :to-be-truthy)
    (expect (getf opts :print-pass-timings) :to-be-null)
    (expect (getf opts :timing-stream) :to-be-null)
    (expect (getf opts :opt-remarks-mode) :to-be :all)
    (expect (getf opts :trace-json-stream) :to-be-null)))

(it-sequential "pipeline-native-make-opts-explicit-values"
  (let ((opts (cl-cc::%make-native-opts :pass-pipeline '(:fold :dce)
                                        :inline-threshold-scale 2
                                        :print-pass-timings t
                                        :opt-remarks-mode :pass)))
    (expect (getf opts :pass-pipeline) :to-equal '(:fold :dce))
    (expect (= 2 (getf opts :inline-threshold-scale)) :to-be-truthy)
    (expect (getf opts :print-pass-timings) :to-be-truthy)
    (expect (getf opts :opt-remarks-mode) :to-be :pass)))

(it-sequential "pipeline-native-make-opts-applyable-to-compile-expression"
  (let ((captured-args nil))
    (with-replaced-function (cl-cc:compile-expression
                             (lambda (form &rest args)
                               (declare (ignore form))
                               (setf captured-args args)
                               (cl-cc/compile:make-compilation-result :program nil)))
      (let ((opts (cl-cc::%make-native-opts :pass-pipeline '(:fold)
                                            :inline-threshold-scale 2)))
        (apply #'cl-cc:compile-expression '(+ 1 2) :target :x86_64 opts)
        (expect (getf captured-args :pass-pipeline) :to-equal '(:fold))
        (expect (= 2 (getf captured-args :inline-threshold-scale)) :to-be-truthy)))))

;;; ─── *compile-cache-root* ───────────────────────────────────────────────────

(it-sequential "pipeline-native-cache-root"
  (let ((str (namestring cl-cc::*compile-cache-root*)))
    (expect (pathnamep cl-cc::*compile-cache-root*) :to-be-truthy)
    (expect (stringp str) :to-be-truthy)
    (expect (search ".cache/cl-cc/native/" str) :to-be-truthy)))

;;; ─── %compile-cache-key ─────────────────────────────────────────────────────

(it-sequential "pipeline-native-cache-key-contains-components x86-64-lisp"
  (destructuring-bind (arch lang arch-str lang-str) (list :x86-64 :lisp "X86-64" "LISP")
    (let ((key (cl-cc::%compile-cache-key "(+ 1 2)" arch lang)))
    (expect (search arch-str key) :to-be-truthy)
    (expect (search lang-str key) :to-be-truthy))))

(it-sequential "pipeline-native-cache-key-contains-components arm64-lisp"
  (destructuring-bind (arch lang arch-str lang-str) (list :arm64 :lisp "ARM64" "LISP")
    (let ((key (cl-cc::%compile-cache-key "(+ 1 2)" arch lang)))
    (expect (search arch-str key) :to-be-truthy)
    (expect (search lang-str key) :to-be-truthy))))

(it-sequential "pipeline-native-cache-key-contains-components x86-64-php"
  (destructuring-bind (arch lang arch-str lang-str) (list :x86-64 :php "X86-64" "PHP")
    (let ((key (cl-cc::%compile-cache-key "(+ 1 2)" arch lang)))
    (expect (search arch-str key) :to-be-truthy)
    (expect (search lang-str key) :to-be-truthy))))

(it-sequential "pipeline-native-cache-key-string-properties"
  (expect (stringp (cl-cc::%compile-cache-key "hello" :x86-64 :lisp)) :to-be-truthy)
  (let ((k1 (cl-cc::%compile-cache-key "(defun f (x) x)" :x86-64 :lisp))
        (k2 (cl-cc::%compile-cache-key "(defun f (x) x)" :x86-64 :lisp)))
    (expect k2 :to-equal k1))
  (let* ((key (cl-cc::%compile-cache-key "test" :x86-64 :lisp))
         (parts (cl:loop for start = 0 then (1+ pos)
                         for pos = (position #\- key :start start)
                         collect (subseq key start (or pos (length key)))
                         while pos)))
    (expect (>= (length parts) 3) :to-be-truthy)))

(it-sequential "pipeline-native-cache-key-differs-by-dimension arch"
  (destructuring-bind (make-variant) (list (lambda () (cl-cc::%compile-cache-key "source" :arm64  :lisp)))
    (let ((baseline (cl-cc::%compile-cache-key "source" :x86-64 :lisp)))
    (expect (equal baseline (funcall make-variant)) :to-be-falsy))))

(it-sequential "pipeline-native-cache-key-differs-by-dimension language"
  (destructuring-bind (make-variant) (list (lambda () (cl-cc::%compile-cache-key "source" :x86-64 :php)))
    (let ((baseline (cl-cc::%compile-cache-key "source" :x86-64 :lisp)))
    (expect (equal baseline (funcall make-variant)) :to-be-falsy))))

(it-sequential "pipeline-native-cache-key-differs-by-dimension content"
  (destructuring-bind (make-variant) (list (lambda () (cl-cc::%compile-cache-key "bbb"    :x86-64 :lisp)))
    (let ((baseline (cl-cc::%compile-cache-key "source" :x86-64 :lisp)))
    (expect (equal baseline (funcall make-variant)) :to-be-falsy))))

(it-sequential "pipeline-native-cache-key-differs-by-dimension options"
  (destructuring-bind (make-variant) (list (lambda () (cl-cc::%compile-cache-key
                                     "source" :x86-64 :lisp
                                     (cl-cc::%make-native-opts :speed 3))))
    (let ((baseline (cl-cc::%compile-cache-key "source" :x86-64 :lisp)))
    (expect (equal baseline (funcall make-variant)) :to-be-falsy))))

(it-sequential "pipeline-native-cache-key-differs-by-dimension inline-threshold"
  (destructuring-bind (make-variant) (list (lambda () (cl-cc::%compile-cache-key
                                            "source" :x86-64 :lisp
                                            (cl-cc::%make-native-opts
                                             :inline-threshold-scale 2))))
    (let ((baseline (cl-cc::%compile-cache-key "source" :x86-64 :lisp)))
    (expect (equal baseline (funcall make-variant)) :to-be-falsy))))

(it-sequential "pipeline-native-cache-key-ignores-observability-options"
  (let ((baseline (cl-cc::%compile-cache-key
                   "source" :x86-64 :lisp
                   (cl-cc::%make-native-opts :speed 3
                                             :inline-threshold-scale 2)))
        (reporting (cl-cc::%compile-cache-key
                    "source" :x86-64 :lisp
                    (cl-cc::%make-native-opts :speed 3
                                              :inline-threshold-scale 2
                                              :print-pass-timings t
                                              :timing-stream *standard-output*
                                              :print-opt-remarks t
                                              :opt-remarks-stream *error-output*
                                              :print-pass-stats t
                                              :stats-stream *standard-output*
                                              :trace-json-stream *standard-output*))))
    (expect reporting :to-equal baseline)))

;;; ─── %compile-cache-path ────────────────────────────────────────────────────

(it-sequential "pipeline-native-cache-path-rooted-under-cache-root"
  (let* ((path     (cl-cc::%compile-cache-path "mykey" #P"a.out"))
         (root-str (namestring cl-cc::*compile-cache-root*))
         (path-str (namestring path)))
    (expect (pathnamep path) :to-be-truthy)
    (expect (and (> (length path-str) (length root-str))
                      (string= root-str (subseq path-str 0 (length root-str)))) :to-be-truthy)))

(it-sequential "pipeline-native-cache-path-embeds-key-in-path"
  (let* ((key "unique-cache-key-42")
         (path (cl-cc::%compile-cache-path key #P"a.out")))
    (expect (search key (namestring path)) :to-be-truthy)))

(it-sequential "pipeline-native-cache-path-different-keys-differ"
  (let ((p1 (cl-cc::%compile-cache-path "key-one" #P"a.out"))
        (p2 (cl-cc::%compile-cache-path "key-two" #P"a.out")))
    (expect (equal (namestring p1) (namestring p2)) :to-be-falsy)))

(it-sequential "pipeline-native-cache-path-preserves-output-filename"
  (let ((path (cl-cc::%compile-cache-path "k" #P"a.out")))
    (expect (pathnamep path) :to-be-truthy)
    (expect (pathname-name path) :to-equal "a")))

(it-sequential "pipeline-native-cache-path-filename-components filename"
  (destructuring-bind (expected accessor) (list "my-program" (lambda (p) (pathname-name p)))
    (let ((path (cl-cc::%compile-cache-path "somekey" #P"my-program.out")))
    (expect (funcall accessor path) :to-equal expected))))

(it-sequential "pipeline-native-cache-path-filename-components extension"
  (destructuring-bind (expected accessor) (list "out" (lambda (p) (pathname-type p)))
    (let ((path (cl-cc::%compile-cache-path "somekey" #P"my-program.out")))
    (expect (funcall accessor path) :to-equal expected))))
