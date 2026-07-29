;;;; run-tests.lisp — the canonical Lisp-level entry point for cl-cc's tests.
;;;;
;;;;   sbcl --script run-tests.lisp [--no-warm-stdlib]
;;;;
;;;; This file holds the test plan itself. `nix run .#test` and `checks.default`
;;;; both reach it; the only thing Nix adds is a `--core` snapshot with :cl-cc,
;;;; :cl-cc-cli and :cl-cc-testing-framework already loaded, which turns a
;;;; multi-minute ASDF compile into a load of pre-built FASLs. The plan below is
;;;; identical either way, so the fast path cannot drift from the plain one.
;;;;
;;;; Everything here is loaded one form at a time, which matters: the package
;;;; prefixes further down (cl-cc/expand:, cl-cc/pipeline::, cl-weave:) do not
;;;; exist until the `asdf:load-system` form has run, and `load` does not read
;;;; ahead.

(require :asdf)
(require :uiop)

;;; Under `--core`, ASDF already knows every cl-cc system and its FASLs, and the
;;; source registry points into the Nix store; re-registering from the working
;;; directory there would discard the pre-built FASLs. So only bootstrap the
;;; registry when the system is genuinely absent, i.e. on a plain
;;; `sbcl --script run-tests.lisp`.
(let ((here (uiop:pathname-directory-pathname
             (or *load-truename* (uiop:getcwd)))))
  (unless (asdf:find-system "cl-cc/test" nil)
    ;; cl-cc.asd registers every packages/*/ system plus "cl-cc/test" and
    ;; "cl-cc/test/e2e" through its own eval-when blocks.
    (asdf:load-asd (merge-pathnames "cl-cc.asd" here))))

;;; *default-pathname-defaults* and uiop:*temporary-directory* are baked into
;;; the core at build-sandbox time; reset both to real runtime values.
;;; uiop:*temporary-directory* must be set via (uiop:temporary-directory) — NOT
;;; nil. Setting it to nil and then passing it as :defaults to make-pathname
;;; hangs SBCL 2.6.1 on macOS ARM64; (uiop:temporary-directory) reads TMPDIR
;;; from the runtime environment and caches the result.
(setf *default-pathname-defaults* (uiop:getcwd))
(setf uiop:*temporary-directory* (uiop:temporary-directory))

;;; The test system is deliberately NOT baked into the core image: test files
;;; have top-level forms that would otherwise capture Nix sandbox paths in
;;; globals. It is loaded fresh, after the working-directory reset above, so any
;;; top-level path computation sees the real CWD.
(format t "# loading cl-cc/test~%")
(handler-case (asdf:load-system "cl-cc/test")
  (error (e)
    (format *error-output* "~&FATAL: ~A~%" e)
    (uiop:quit 1)))

;;; Reset *macro-eval-fn* to a mutex-wrapped #'eval so test bodies run under the
;;; host CL, not the cl-cc VM. pipeline-selfhost.lisp sets it to #'our-eval at
;;; load time; leaving it as our-eval causes sb-ext:with-timeout interrupts to be
;;; swallowed inside the VM loop, hanging test workers indefinitely.
;;;
;;; The mutex serialises concurrent SBCL compiler invocations: on macOS 26 ARM64
;;; with SBCL 2.6.1, four or more parallel workers calling eval simultaneously
;;; (via compiler-macro expansion in invoke-registered-expander) deadlock on GC
;;; safepoints while the SBCL compiler holds internal locks. The lock is captured
;;; by the closure; %with-isolated-macro-environment identity-rebinds
;;; *macro-eval-fn* so all threads share the same mutex.
(let ((lock (sb-thread:make-mutex :name "macro-eval-lock")))
  (setf cl-cc/expand:*macro-eval-fn*
        (lambda (form) (sb-thread:with-mutex (lock) (eval form)))))

;;; The core image is produced in a Nix build sandbox, so the baked stdlib
;;; disk-cache path may point under /nix/var/nix/builds. Rebase it to the
;;; caller's runtime HOME before the optional warm step.
(let ((home (uiop:getenv "HOME")))
  (when home
    (setf cl-cc/pipeline::*stdlib-cache-directory*
          (merge-pathnames #P".cache/cl-cc/"
                           (uiop:ensure-directory-pathname (pathname home))))))

(format t "# starting fast test plan (unit)~%")

;;; Pre-warm BOTH stdlib caches in the main thread (single-threaded, safe). The
;;; core bakes *stdlib-expanded-cache-eval-fn* = #'our-eval and a
;;; *stdlib-vm-snapshot* compiled under our-eval. After the *macro-eval-fn* reset
;;; above, both caches are stale. Without pre-warming, all parallel workers
;;; simultaneously see cache misses and race to rebuild unprotected globals
;;; (*stdlib-expanded-cache*, *stdlib-vm-snapshot*, ...). On macOS ARM64 SBCL,
;;; concurrent large allocations (857-line stdlib) during GC safepoint windows
;;; can deadlock threads waiting on watchdog-lock. warm-stdlib-cache rebuilds
;;; both caches under *macro-eval-fn* = #'eval so workers see cache HITs and
;;; bypass both rebuild paths entirely.
;;;
;;; Skippable with --no-warm-stdlib or CLCC_WARM_STDLIB=0, which is how the
;;; warm path itself gets exercised as a code path rather than assumed.
(let* ((args (uiop:command-line-arguments))
       (filter (loop for rest on args
                     when (and (string= (first rest) "--filter")
                               (second rest))
                       return (second rest)))
       (warm-env (uiop:getenv "CLCC_WARM_STDLIB"))
       (worker-env (uiop:getenv "CL_CC_TEST_WORKERS"))
       (max-workers
         (when worker-env
           (let ((value (parse-integer worker-env)))
             (unless (plusp value)
               (error "CL_CC_TEST_WORKERS must be a positive integer, got ~S."
                      worker-env))
             value)))
       (warm-stdlib (and (not (member "--no-warm-stdlib" args :test #'string=))
                         (not (and warm-env
                                   (member (string-downcase warm-env)
                                           '("0" "false" "no" "off")
                                           :test #'string=))))))
  (handler-case
      (progn
        (if warm-stdlib
            (progn
              (format t "# warming stdlib cache~%")
              (cl-cc:warm-stdlib-cache)
              (format t "# stdlib cache ready~%"))
            (progn
              (format t "# stdlib cache warm skipped~%")
              (format t "# forcing serial test execution (max-workers=1) because stdlib cache warming is disabled~%")
              (setf max-workers 1)))
        ;; cl-weave is the test engine: registration, execution, reporting and
        ;; concurrency all delegate to it (see
        ;; packages/testing-framework/src/framework-definitions.lisp).
        (uiop:quit
         (if (cl-weave:run-all :reporter :spec
                               :name-filter filter
                               :max-workers max-workers)
             0
             1)))
    (error (e)
      (format t "~&not ok - run-all fatal error: ~A~%" e)
      (format *error-output* "~&FATAL: ~A~%" e)
      (uiop:quit 1))))
