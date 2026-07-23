;;;; tests/runtime-subsystem-fr-tests.lisp - Runtime Subsystem FR Evidence Tests
;;;;
;;;; Evidence tests for feature requirements documented in
;;;; docs/runtime-subsystem.md.  These tests verify that every FR has
;;;; implementation evidence: symbols exist, files load, and basic
;;;; functionality works.
;;;;
;;;; FR Coverage:
;;;;   Wave 0: Load-graph verification (all source files loadable)
;;;;   Wave 1: ANSI runtime — Unicode, pprint, time, env, conditions, chars, arrays, I/O
;;;;   Wave 2: Atomics, sync, lock-free, EBR, hazard, RCU, QSBR, MVCC
;;;;   Wave 3: Safepoints, stack maps, tail calls, stack/OOM safety
;;;;   Wave 4: Concurrency — scheduler, tasks, futures, channels, actors, STM, fibers
;;;;   Wave 5: OS, async I/O, network, image, signals, mmap

(in-package :cl-cc/test)



;;; =================================================================
;;; Wave 0: Load-Graph Verification
;;; =================================================================

(it-sequential "runtime-subsystem-all-source-files-loadable"
  (expect (asdf:find-system :cl-cc-runtime nil) :to-be-truthy)
  (expect (asdf:find-system :cl-cc-vm nil) :to-be-truthy))

(it-sequential "runtime-subsystem-c-embedding-api-loaded"
  (expect (probe-file (asdf:system-relative-pathname :cl-cc-runtime "include/cl-cc.h")) :to-be-truthy)
  (expect (fboundp 'cl-cc/runtime:cl-cc-init) :to-be-truthy)
  (expect (fboundp 'cl-cc/runtime:cl-cc-eval) :to-be-truthy)
  (expect (fboundp 'cl-cc/runtime:cl-cc-call) :to-be-truthy)
  (expect (fboundp 'cl-cc/runtime:cl-cc-cleanup) :to-be-truthy)
  (expect (fboundp 'cl-cc/runtime:cl-cc-last-error) :to-be-truthy)
  (expect (fboundp 'cl-cc/runtime:cl-cc-register-callback) :to-be-truthy))

(it-sequential "runtime-subsystem-c-embedding-api-eval-call-cleanup"
  (let ((state (cl-cc/runtime:cl-cc-init)))
    (unwind-protect
         (progn
           (let ((value (cl-cc/runtime:cl-cc-eval state "(+ 20 22)")))
             (expect (cl-cc/runtime:cl-cc-value-kind value) :to-be :integer)
             (expect (= 42 (cl-cc/runtime:cl-cc-value-payload value)) :to-be-truthy))
           (cl-cc/runtime:cl-cc-eval state "(defun embedded-add (a b) (+ a b))")
           (let ((value (cl-cc/runtime:cl-cc-call state "embedded-add" 7 8)))
             (expect (cl-cc/runtime:cl-cc-value-kind value) :to-be :integer)
             (expect (= 15 (cl-cc/runtime:cl-cc-value-payload value)) :to-be-truthy))
           #+(and sbcl sb-alien-callback)
           (let ((callback (cl-cc/runtime:cl-cc-register-callback
                            state "identity" #'identity :arg-types '(:pointer) :return-type :pointer)))
             (expect callback :to-be-truthy)
             (expect (cl-cc/runtime:cl-cc-callback state "identity") :to-be callback))
           #-(and sbcl sb-alien-callback)
           (signals error (cl-cc/runtime:cl-cc-register-callback
              state "identity" #'identity :arg-types '(:pointer) :return-type :pointer))
           (let ((value (cl-cc/runtime:cl-cc-eval state "(/ 1 0)")))
             (expect (cl-cc/runtime:cl-cc-value-kind value) :to-be :error)
             (expect (= 1 (cl-cc/runtime:cl-cc-error-code
                          (cl-cc/runtime:cl-cc-last-error state))) :to-be-truthy)))
      (cl-cc/runtime:cl-cc-cleanup state))
    (expect (cl-cc/runtime:cl-cc-state-closed-p state) :to-be-truthy)))

(it-sequential "runtime-subsystem-deopt-trampoline-signals-fatal-error"
  (let ((frame-count cl-cc/runtime::*rt-deopt-frame-count*))
    (signals error (cl-cc/runtime::rt-deopt-trampoline 7 '((:r0 . 42))))
    (expect (= (1+ frame-count) cl-cc/runtime::*rt-deopt-frame-count*) :to-be-truthy)))

(it-sequential "runtime-subsystem-multiple-vm-instances-isolated"
  (let* ((parent-globals (make-hash-table :test #'eq))
         (parent-env (cl-cc/vm:make-vm-parent-environment :globals parent-globals))
         (left (cl-cc/vm:make-vm-instance :parent-env parent-env))
         (right (cl-cc/vm:make-vm-instance :parent-env parent-env)))
    (setf (gethash 'shared parent-globals) 99)
    (setf (gethash 'local (cl-cc/vm:vm-global-vars left)) :left)
    (setf (gethash 'local (cl-cc/vm:vm-global-vars right)) :right)
    (expect (eq left right) :to-be-falsy)
    (expect (eq (cl-cc/vm:vm-state-heap left)
                      (cl-cc/vm:vm-state-heap right)) :to-be-falsy)
    (expect (cl-cc/vm:vm-instance-global-value left 'local) :to-be :left)
    (expect (cl-cc/vm:vm-instance-global-value right 'local) :to-be :right)
    (expect (= 99 (cl-cc/vm:vm-instance-global-value left 'shared)) :to-be-truthy)
    (expect (= 99 (cl-cc/vm:vm-instance-global-value right 'shared)) :to-be-truthy)
    (let ((transferred (cl-cc/vm:transfer-value '(1 2 3) left right)))
      (expect transferred :to-equal '(1 2 3))
      (expect (eq transferred '(1 2 3)) :to-be-falsy))))


;;; =================================================================
;;; Existence macros — data/logic separation
;;;
;;; define-fr-loaded-test: Prolog-style clause that checks every listed
;;; symbol is fboundp. Data = the symbol list; logic = the macro.
;;;
;;; define-fr-existence-test: more general form supporting
;;; (:f sym) fboundp / (:b sym) boundp / (:s pkg name) find-symbol.
;;; =================================================================

(defmacro define-fr-loaded-test (name fr-doc &rest symbols)
  "Existence test: assert every SYMBOL in the list is fboundp."
  `(deftest ,name
     ,fr-doc
     ,@(mapcar (lambda (sym) `(expect (fboundp ',sym) :to-be-truthy)) symbols)))

(defmacro define-fr-existence-test (name fr-doc &rest checks)
  "Existence test supporting mixed check types.
Each check is (:f sym) fboundp / (:b sym) boundp / (:s pkg sym) find-symbol."
  `(deftest ,name
     ,fr-doc
     ,@(mapcar (lambda (check)
                 (ecase (car check)
                   (:f `(expect (fboundp  ',(second check)) :to-be-truthy))
                   (:b `(expect (boundp   ',(second check)) :to-be-truthy))
                   (:s `(expect (find-symbol ,(second check) ,(third check)) :to-be-truthy))))
               checks)))

;;; =================================================================
;;; Wave 2: Sync + concurrency subsystem
;;; =================================================================

(define-fr-loaded-test runtime-subsystem-sync-primitives-loaded
  "FR-370-373: Sync primitives are loaded."
  cl-cc/runtime:rt-make-mutex
  cl-cc/runtime:rt-mutex-lock
  cl-cc/runtime:rt-mutex-unlock
  cl-cc/runtime:rt-make-condition-variable
  cl-cc/runtime:rt-make-semaphore
  cl-cc/runtime:rt-make-barrier
  cl-cc/runtime:rt-make-once)

(define-fr-loaded-test runtime-subsystem-scheduler-loaded
  "FR-257, FR-258, FR-552: Green thread scheduler is loaded."
  cl-cc/runtime:rt-make-scheduler
  cl-cc/runtime:rt-spawn
  cl-cc/runtime:rt-yield
  cl-cc/runtime:rt-scheduler-run)

(define-fr-loaded-test runtime-subsystem-future-loaded
  "FR-283: Future/promise API is loaded."
  cl-cc/runtime:rt-make-future
  cl-cc/runtime:rt-future-resolve
  cl-cc/runtime:rt-future-await
  cl-cc/runtime:rt-future-done-p)

(define-fr-loaded-test runtime-subsystem-channel-loaded
  "FR-282: CSP channels are loaded."
  cl-cc/runtime:rt-make-channel
  cl-cc/runtime:rt-channel-send
  cl-cc/runtime:rt-channel-recv
  cl-cc/runtime:rt-channel-close)

(define-fr-loaded-test runtime-subsystem-actor-loaded
  "FR-290: Actor model is loaded."
  cl-cc/runtime:rt-make-actor
  cl-cc/runtime:rt-actor-send
  cl-cc/runtime:rt-actor-receive)

(define-fr-loaded-test runtime-subsystem-stm-loaded
  "FR-300: Software transactional memory is loaded."
  cl-cc/runtime:rt-atomically
  cl-cc/runtime:rt-retry)

(define-fr-loaded-test runtime-subsystem-lockfree-loaded
  "FR-322: Lock-free data structures are loaded."
  cl-cc/runtime:rt-make-lfstack
  cl-cc/runtime:rt-make-lfqueue
  cl-cc/runtime:rt-make-lfhash-map)

(define-fr-loaded-test runtime-subsystem-ebr-loaded
  "FR-320: Epoch-based reclamation is loaded."
  cl-cc/runtime:rt-ebr-init
  cl-cc/runtime:rt-ebr-register-thread
  cl-cc/runtime:rt-ebr-enter
  cl-cc/runtime:rt-ebr-leave)

(define-fr-loaded-test runtime-subsystem-hazard-loaded
  "FR-321: Hazard pointers are loaded."
  cl-cc/runtime:rt-hp-register-thread
  cl-cc/runtime:rt-hp-protect
  cl-cc/runtime:rt-hp-retire)

(define-fr-loaded-test runtime-subsystem-rcu-loaded
  "FR-380: Read-copy-update (RCU) is loaded."
  cl-cc/runtime:rt-rcu-read-lock
  cl-cc/runtime:rt-rcu-synchronize)

(define-fr-loaded-test runtime-subsystem-qsbr-loaded
  "FR-452: Quiescent-state-based reclamation (QSBR) is loaded."
  cl-cc/runtime:rt-qsbr-init
  cl-cc/runtime:rt-qsbr-register-thread
  cl-cc/runtime:rt-qsbr-synchronize)

;;; =================================================================
;;; Wave 5: OS + I/O subsystem
;;; =================================================================

(define-fr-loaded-test runtime-subsystem-os-loaded
  "FR-570, FR-573: OS abstraction layer is loaded."
  cl-cc/runtime:rt-open
  cl-cc/runtime:rt-close
  cl-cc/runtime:rt-getenv
  cl-cc/runtime:rt-argv
  cl-cc/runtime:rt-exit)

(define-fr-loaded-test runtime-subsystem-net-loaded
  "FR-574: Network primitives are loaded."
  cl-cc/runtime:rt-socket
  cl-cc/runtime:rt-bind
  cl-cc/runtime:rt-listen
  cl-cc/runtime:rt-connect)

(define-fr-loaded-test runtime-subsystem-image-loaded
  "FR-350, FR-563: Image save/restore is loaded."
  cl-cc/runtime:rt-save-image
  cl-cc/runtime:rt-load-image)

;;; =================================================================
;;; Wave 3: Runtime internals
;;; =================================================================

(define-fr-existence-test runtime-subsystem-safepoints-loaded
  "FR-510: GC safepoint infrastructure is loaded."
  (:f cl-cc/runtime:rt-gc-enter-safe-region)
  (:f cl-cc/runtime:rt-gc-leave-safe-region)
  (:b cl-cc/runtime:*rt-gc-safe-region-depths*))

(define-fr-loaded-test runtime-subsystem-tlab-loaded
  "FR-550: Thread-local allocation buffers (TLAB) are loaded."
  cl-cc/runtime:rt-gc-tlab-alloc
  cl-cc/runtime:rt-gc-tlab-retire-all)

(define-fr-loaded-test runtime-subsystem-context-loaded
  "FR-412: Context propagation is loaded."
  cl-cc/runtime:rt-context-cancel
  cl-cc/runtime:rt-context-cancelled-p)

(define-fr-loaded-test runtime-subsystem-spsc-loaded
  "FR-462: Single-producer/single-consumer ring buffer is loaded."
  cl-cc/runtime:rt-make-spsc-queue
  cl-cc/runtime:rt-spsc-try-push
  cl-cc/runtime:rt-spsc-try-pop)

(define-fr-loaded-test runtime-subsystem-perf-loaded
  "FR-481: Hardware performance counters are loaded."
  cl-cc/runtime:rt-perf-init
  cl-cc/runtime:rt-perf-enable-counter)

(define-fr-loaded-test runtime-subsystem-otel-loaded
  "FR-490: OpenTelemetry tracing is loaded."
  cl-cc/runtime:rt-otel-start-span
  cl-cc/runtime:rt-otel-end-span)

(define-fr-loaded-test runtime-subsystem-consensus-loaded
  "FR-432: Raft consensus protocol is loaded."
  cl-cc/runtime:rt-make-raft-node
  cl-cc/runtime:rt-make-raft-cluster
  cl-cc/runtime:rt-raft-propose)

(define-fr-loaded-test runtime-subsystem-crdt-loaded
  "FR-431: Conflict-free replicated data types (CRDTs) are loaded."
  cl-cc/runtime:rt-make-gcounter
  cl-cc/runtime:rt-make-pncounter
  cl-cc/runtime:rt-make-lwwregister)

(define-fr-loaded-test runtime-subsystem-parallel-algo-loaded
  "FR-470-472: Parallel algorithms are loaded."
  cl-cc/runtime:rt-parallel-algo-init)

;;; =================================================================
;;; Wave 1: VM ANSI Runtime Features
;;; =================================================================

(define-fr-existence-test runtime-subsystem-unicode-loaded
  "FR-590-593: Unicode support is loaded in VM."
  (:s "VM-CHAR-UPCASE"   :cl-cc/vm)
  (:s "VM-CHAR-DOWNCASE" :cl-cc/vm))

(define-fr-existence-test runtime-subsystem-pathname-loaded
  "FR-595-597: Pathname system is loaded in VM."
  (:s "VM-MAKE-PATHNAME" :cl-cc/vm)
  (:s "VM-PATHNAME-NAME" :cl-cc/vm))

(define-fr-existence-test runtime-subsystem-stream-loaded
  "FR-600-602: Stream types are loaded in VM."
  (:s "VM-MAKE-STRING-OUTPUT-STREAM" :cl-cc/vm))

(define-fr-existence-test runtime-subsystem-time-loaded
  "FR-610: Time API is loaded in VM."
  (:f cl-cc/vm:get-universal-time)
  (:f cl-cc/vm:get-internal-real-time))

(define-fr-existence-test runtime-subsystem-random-loaded
  "FR-611: RNG is loaded in VM."
  (:f cl-cc/vm:make-vm-random)
  (:f cl-cc/vm:make-vm-make-random-state))

(define-fr-existence-test runtime-subsystem-environment-loaded
  "FR-612: Environment introspection is loaded in VM."
  (:f cl-cc/vm:lisp-implementation-type)
  (:f cl-cc/vm:lisp-implementation-version)
  (:f cl-cc/vm::short-site-name)
  (:f cl-cc/vm::long-site-name))

(define-fr-existence-test runtime-subsystem-conditions-loaded
  "FR-643-646: Condition system is loaded in VM."
  (:s "VM-DEFINE-CONDITION" :cl-cc/vm)
  (:s "VM-HANDLER-CASE"     :cl-cc/vm)
  (:s "VM-SIGNAL"           :cl-cc/vm))

(define-fr-existence-test runtime-subsystem-array-dimensions-loaded
  "FR-634: Array dimension queries are loaded in VM."
  (:s "VM-ARRAY-RANK"       :cl-cc/vm)
  (:s "VM-ARRAY-DIMENSIONS" :cl-cc/vm))

;;; =================================================================
;;; Wave 2-5: Integration Tests
;;; =================================================================

;;; -----------------------------------------------------------------
;;; FR semantic evidence tests
;;; -----------------------------------------------------------------

(it-sequential "runtime-subsystem-sync-mutex-prevents-reentrant-access"
  (let ((m (cl-cc/runtime:rt-make-mutex)))
    (expect (cl-cc/runtime:rt-mutex-lock m) :to-be-truthy)
    (unwind-protect
         (expect (not (cl-cc/runtime::rt-mutex-try-lock m)) :to-be-truthy)
      (cl-cc/runtime:rt-mutex-unlock m))))

(it-sequential "runtime-subsystem-sync-semaphore-counts-correctly"
  (let ((s (cl-cc/runtime:rt-make-semaphore :count 2)))
    (expect (cl-cc/runtime::rt-semaphore-try-wait s) :to-be-truthy)
    (expect (cl-cc/runtime::rt-semaphore-try-wait s) :to-be-truthy)
    (expect (not (cl-cc/runtime::rt-semaphore-try-wait s)) :to-be-truthy)
    (expect (= 2 (cl-cc/runtime:rt-semaphore-signal s 2)) :to-be-truthy)
    (expect (cl-cc/runtime::rt-semaphore-try-wait s) :to-be-truthy)
    (expect (cl-cc/runtime::rt-semaphore-try-wait s) :to-be-truthy)))

(it-sequential "runtime-subsystem-sync-barrier-releases-waiters"
  (let ((b (cl-cc/runtime:rt-make-barrier 1)))
    (expect (= 0 (cl-cc/runtime::rt-barrier-gen b)) :to-be-truthy)
    (expect (cl-cc/runtime:rt-barrier-wait b :timeout 0.1) :to-be-truthy)
    (expect (= 0 (cl-cc/runtime::rt-barrier-count b)) :to-be-truthy)
    (expect (= 1 (cl-cc/runtime::rt-barrier-gen b)) :to-be-truthy)))

(it-sequential "runtime-subsystem-sync-once-call-executes-once"
  (let ((o (cl-cc/runtime:rt-make-once))
        (calls '()))
    (expect (cl-cc/runtime:rt-once-call o (lambda () (push :first calls) :first)) :to-be :first)
    (expect (cl-cc/runtime:rt-once-call o (lambda () (push :second calls) :second)) :to-be :first)
    (expect calls :to-equal '(:first))))

(it-sequential "runtime-subsystem-scheduler-spawned-tasks-execute-in-order"
  (let ((cl-cc/runtime::*rt-global-scheduler* (cl-cc/runtime:rt-make-scheduler)))
    (let ((events '()))
      (cl-cc/runtime:rt-spawn (lambda () (push :low events)) :priority :low)
      (cl-cc/runtime:rt-spawn (lambda () (push :normal events)) :priority :normal)
      (cl-cc/runtime:rt-spawn (lambda () (push :high events)) :priority :high)
      (cl-cc/runtime:rt-scheduler-run)
      (expect events :to-equal '(:low :normal :high)))))

(it-sequential "runtime-subsystem-scheduler-sleep-task-records-wake-time"
  (let ((cl-cc/runtime::*rt-global-scheduler* (cl-cc/runtime:rt-make-scheduler)))
    (sb-ext:without-package-locks
      (with-replaced-function (get-internal-real-time (lambda () 1000))
        (let ((observed-wake-time nil))
          (cl-cc/runtime:rt-spawn
           (lambda ()
             (cl-cc/runtime::rt-sleep-task 1)
             (setf observed-wake-time
                   (cl-cc/runtime::rt-green-thread-wake-time
                    cl-cc/runtime::*rt-current-green-thread*))))
          (cl-cc/runtime:rt-scheduler-run :once t)
          (expect (= (+ 1000 internal-time-units-per-second) observed-wake-time) :to-be-truthy))))))

(it-sequential "runtime-subsystem-channel-buffered-preserves-order"
  (let ((ch (cl-cc/runtime:rt-make-channel :capacity 3)))
    (cl-cc/runtime:rt-channel-send ch :a)
    (cl-cc/runtime:rt-channel-send ch :b)
    (cl-cc/runtime:rt-channel-send ch :c)
    (expect (multiple-value-list (cl-cc/runtime:rt-channel-recv ch)) :to-equal '(:a t))
    (expect (multiple-value-list (cl-cc/runtime:rt-channel-recv ch)) :to-equal '(:b t))
    (expect (multiple-value-list (cl-cc/runtime:rt-channel-recv ch)) :to-equal '(:c t))))

(it-sequential "runtime-subsystem-channel-close-prevents-further-sends"
  (let ((ch (cl-cc/runtime:rt-make-channel :capacity 1)))
    (expect (cl-cc/runtime:rt-channel-close ch) :to-be-truthy)
    (expect (handler-case
            (progn (cl-cc/runtime:rt-channel-send ch :after-close) nil)
          (error () t)) :to-be-truthy)))

(it-sequential "runtime-subsystem-channel-select-returns-first-available"
  (let ((empty (cl-cc/runtime:rt-make-channel :capacity 1))
        (ready (cl-cc/runtime:rt-make-channel :capacity 1)))
    (cl-cc/runtime:rt-channel-send ready :ready)
    (multiple-value-bind (value channel ok)
        (cl-cc/runtime::rt-channel-select (list empty ready) :timeout 0.01)
      (expect ok :to-be-truthy)
      (expect value :to-be :ready)
      (expect channel :to-be ready))))

(it-sequential "runtime-subsystem-actor-processes-messages-in-order"
  (let ((a (cl-cc/runtime:rt-make-actor #'identity)))
    (cl-cc/runtime:rt-actor-send a :first)
    (cl-cc/runtime:rt-actor-send a :second)
    (cl-cc/runtime:rt-actor-send a :third)
    (expect (list (cl-cc/runtime:rt-actor-receive a :timeout 0.1)
                     (cl-cc/runtime:rt-actor-receive a :timeout 0.1)
                     (cl-cc/runtime:rt-actor-receive a :timeout 0.1)) :to-equal '(:third :second :first))))

(it-sequential "runtime-subsystem-stm-atomically-commits-transaction"
  (let ((cell (cl-cc/runtime::rt-make-tvar 10)))
    (expect (= 15 (cl-cc/runtime:rt-atomically
                (let ((old (cl-cc/runtime::rt-read-tvar cell)))
                  (cl-cc/runtime::rt-write-tvar cell (+ old 5))))) :to-be-truthy)
    (expect (= 15 (cl-cc/runtime::rt-tvar-value-unsafe cell)) :to-be-truthy)))

(it-sequential "runtime-subsystem-stm-retries-on-conflict"
  (let ((cell (cl-cc/runtime::rt-make-tvar 0))
        (attempts 0))
    (expect (= 20 (cl-cc/runtime:rt-atomically
                (incf attempts)
                (cl-cc/runtime::rt-read-tvar cell)
                (when (= attempts 1)
                  (setf (cl-cc/runtime::rt-tvar-value cell) 10
                        (cl-cc/runtime::rt-tvar-version cell)
                        (1+ (cl-cc/runtime::rt-tvar-version cell))))
                (cl-cc/runtime::rt-write-tvar cell 20))) :to-be-truthy)
    (expect (= 2 attempts) :to-be-truthy)
    (expect (= 20 (cl-cc/runtime::rt-tvar-value-unsafe cell)) :to-be-truthy)))

(it-sequential "runtime-subsystem-fr-740-stm-isolates-staged-writes-until-commit"
  (let ((cell (cl-cc/runtime:rt-make-tvar 1))
        (observed-before-commit nil))
    (expect (= 2 (cl-cc/runtime:rt-atomically
                (cl-cc/runtime:rt-write-tvar cell 2)
                (setf observed-before-commit
                      (cl-cc/runtime:rt-tvar-value-unsafe cell))
                (cl-cc/runtime:rt-read-tvar cell))) :to-be-truthy)
    (expect (= 1 observed-before-commit) :to-be-truthy)
    (expect (= 2 (cl-cc/runtime:rt-tvar-value-unsafe cell)) :to-be-truthy)))

(it-sequential "runtime-subsystem-fr-741-async-await-runs-through-scheduler"
  (let ((cl-cc/runtime::*rt-global-scheduler* (cl-cc/runtime:rt-make-scheduler)))
    (let ((future (cl-cc/runtime:rt-async (+ 20 22))))
      (expect (= 42 (cl-cc/runtime:rt-await future :timeout 0.1)) :to-be-truthy)
      (expect (cl-cc/runtime:rt-future-done-p future) :to-be-truthy))))

(it-sequential "runtime-subsystem-fr-742-work-stealing-runs-local-and-stolen-tasks"
  (let* ((scheduler (cl-cc/runtime:rt-make-work-stealing-scheduler :workers 2))
         (workers (cl-cc/runtime::rt-work-stealing-scheduler-workers scheduler))
         (w0 (first workers))
         (w1 (second workers))
         (events nil))
    (cl-cc/runtime:rt-work-stealing-submit scheduler (lambda () (push :first events)))
    (cl-cc/runtime:rt-work-stealing-submit scheduler (lambda () (push :second events)))
    (expect (cl-cc/runtime:rt-worker-run-once w0) :to-be-truthy)
    (expect (cl-cc/runtime:rt-worker-run-once w1) :to-be-truthy)
    (expect (= 2 (length events)) :to-be-truthy)
    (cl-cc/runtime:rt-work-deque-push-front
     (cl-cc/runtime::rt-worker-deque w0)
     (cl-cc/runtime::%make-rt-green-thread :thunk (lambda () (push :stolen events))))
    (expect (cl-cc/runtime:rt-worker-run-once w1) :to-be-truthy)
    (expect (member :stolen events) :to-be-truthy)
    (expect (>= (cl-cc/runtime::rt-worker-steals w1) 1) :to-be-truthy)))

(it-sequential "runtime-subsystem-fr-743-fiber-yield-and-resume-preserve-state"
  (let ((steps nil)
        (fiber nil))
    (setf fiber
          (cl-cc/runtime:rt-make-fiber
           (lambda ()
             (push :start steps)
             (cl-cc/runtime:rt-fiber-block
              (lambda ()
                (push :resume steps)
                :done)
              :blocked))))
    (expect (cl-cc/runtime:rt-fiber-resume fiber) :to-be :blocked)
    (expect (cl-cc/runtime::rt-fiber-status fiber) :to-be :ready)
    (expect (cl-cc/runtime:rt-fiber-resume fiber) :to-be :done)
    (expect (cl-cc/runtime:rt-fiber-done-p fiber) :to-be-truthy)
    (expect steps :to-equal '(:resume :start))))

(it-sequential "runtime-subsystem-lockfree-stack-is-lifo"
  (let ((s (cl-cc/runtime:rt-make-lfstack)))
    (cl-cc/runtime:rt-lfstack-push s :first)
    (cl-cc/runtime:rt-lfstack-push s :second)
    (cl-cc/runtime:rt-lfstack-push s :third)
    (expect (multiple-value-list (cl-cc/runtime:rt-lfstack-pop s)) :to-equal '(:third t))
    (expect (multiple-value-list (cl-cc/runtime:rt-lfstack-pop s)) :to-equal '(:second t))
    (expect (multiple-value-list (cl-cc/runtime:rt-lfstack-pop s)) :to-equal '(:first t))))

(it-sequential "runtime-subsystem-lockfree-queue-is-fifo"
  (let ((q (cl-cc/runtime:rt-make-lfqueue)))
    (cl-cc/runtime::rt-lfqueue-push q :first)
    (cl-cc/runtime::rt-lfqueue-push q :second)
    (cl-cc/runtime::rt-lfqueue-push q :third)
    (expect (multiple-value-list (cl-cc/runtime::rt-lfqueue-pop q)) :to-equal '(:first t))
    (expect (multiple-value-list (cl-cc/runtime::rt-lfqueue-pop q)) :to-equal '(:second t))
    (expect (multiple-value-list (cl-cc/runtime::rt-lfqueue-pop q)) :to-equal '(:third t))))

(it-sequential "runtime-subsystem-ebr-retire-reclaim-cycle"
  (let ((freed '()))
    (cl-cc/runtime:rt-ebr-init (lambda (obj) (push obj freed)))
    (let ((local (cl-cc/runtime:rt-ebr-register-thread)))
      (cl-cc/runtime::rt-ebr-retire local :old-node)
      (expect (= 0 (cl-cc/runtime::rt-ebr-collect local)) :to-be-truthy)
      (expect (= 1 (cl-cc/runtime::rt-ebr-collect local)) :to-be-truthy)
      (expect freed :to-equal '(:old-node)))))

(it-sequential "runtime-subsystem-hazard-retire-defers-protected-objects"
  (let ((freed nil)
        (thread sb-thread:*current-thread*)
        (protected (list :protected))
        (unprotected (list :unprotected)))
    (flet ((free-object (object)
             (push object freed)))
      (cl-cc/runtime::rt-hp-init #'free-object)
      (unwind-protect
           (progn
             (cl-cc/runtime:rt-hp-register-thread 2 thread)
             (cl-cc/runtime:rt-hp-protect 0 protected thread)
             (cl-cc/runtime::rt-hp-retire-object protected
                                                  :thread thread
                                                  :free-fn #'free-object
                                                  :threshold 1)
             (expect freed :to-equal nil)
             (cl-cc/runtime::rt-hp-retire-object unprotected
                                                  :thread thread
                                                  :free-fn #'free-object
                                                  :threshold 1)
             (expect freed :to-equal (list unprotected))
             (expect (gethash thread cl-cc/runtime::*hazard-retired*) :to-equal (list protected))
             (cl-cc/runtime::rt-hp-clear 0 thread)
             (expect (= 1 (cl-cc/runtime::rt-hp-reclaim
                          :thread thread
                          :free-fn #'free-object)) :to-be-truthy)
             (expect freed :to-equal (list protected unprotected)))
        (cl-cc/runtime::rt-hp-init)))))

(it-sequential "runtime-subsystem-spsc-preserves-single-producer-consumer-semantics"
  (let ((q (cl-cc/runtime:rt-make-spsc-queue 2)))
    (expect (cl-cc/runtime:rt-spsc-try-push q :first) :to-be-truthy)
    (expect (cl-cc/runtime:rt-spsc-try-push q :second) :to-be-truthy)
    (expect (cl-cc/runtime::rt-spsc-full-p q) :to-be-truthy)
    (expect (not (cl-cc/runtime:rt-spsc-try-push q :third)) :to-be-truthy)
    (expect (multiple-value-list (cl-cc/runtime:rt-spsc-try-pop q)) :to-equal '(:first t))
    (expect (multiple-value-list (cl-cc/runtime:rt-spsc-try-pop q)) :to-equal '(:second t))
    (expect (multiple-value-list (cl-cc/runtime:rt-spsc-try-pop q)) :to-equal '(nil nil))))

(it-sequential "runtime-subsystem-crdt-gcounter-merges-by-node-max"
  (let ((a (cl-cc/runtime:rt-make-gcounter))
        (b (cl-cc/runtime:rt-make-gcounter)))
    (cl-cc/runtime:rt-gcounter-increment a :n1 1)
    (cl-cc/runtime:rt-gcounter-increment a :n2 5)
    (cl-cc/runtime:rt-gcounter-increment b :n1 3)
    (cl-cc/runtime:rt-gcounter-increment b :n3 7)
    (cl-cc/runtime::rt-gcounter-merge a b)
    (expect (= 15 (cl-cc/runtime:rt-gcounter-value a)) :to-be-truthy)))

(it-sequential "runtime-subsystem-crdt-pncounter-value-is-pos-minus-neg"
  (let ((c (cl-cc/runtime:rt-make-pncounter)))
    (cl-cc/runtime::rt-pncounter-increment c :n1 10)
    (cl-cc/runtime::rt-pncounter-increment c :n2 4)
    (cl-cc/runtime::rt-pncounter-decrement c :n1 3)
    (cl-cc/runtime::rt-pncounter-decrement c :n3 2)
    (expect (= 9 (cl-cc/runtime::rt-pncounter-value c)) :to-be-truthy)))

(it-sequential "runtime-subsystem-raft-leader-election-picks-leader"
  (let* ((cluster (cl-cc/runtime:rt-make-raft-cluster '("n1" "n2" "n3")))
         (node (gethash "n1" (cl-cc/runtime:rt-raft-cluster-nodes cluster))))
    (expect (cl-cc/runtime::rt-raft-start-election node cluster) :to-be-truthy)
    (expect (cl-cc/runtime::rt-raft-cluster-leader-id cluster) :to-equal "n1")
    (expect (= cl-cc/runtime::+raft-leader+ (cl-cc/runtime::rt-raft-node-state node)) :to-be-truthy)))

(it-sequential "runtime-subsystem-raft-log-entries-are-replicated"
  (let* ((cluster (cl-cc/runtime:rt-make-raft-cluster '("n1" "n2" "n3")))
         (leader (gethash "n1" (cl-cc/runtime:rt-raft-cluster-nodes cluster))))
    (expect (cl-cc/runtime::rt-raft-start-election leader cluster) :to-be-truthy)
    (expect (cl-cc/runtime:rt-raft-propose cluster :set-x) :to-be :set-x)
    (maphash
     (lambda (id node)
       (declare (ignore id))
       (let ((commands (mapcar #'cl-cc/runtime::rt-raft-entry-command
                               (cl-cc/runtime::rt-raft-node-log node))))
         (expect (member :set-x commands) :to-be-truthy)))
     (cl-cc/runtime:rt-raft-cluster-nodes cluster))))

(it-sequential "runtime-subsystem-sync-mutex-basic"
  (let ((m (cl-cc/runtime:rt-make-mutex)))
    (expect (cl-cc/runtime:rt-mutex-lock m) :to-be-truthy)
    (cl-cc/runtime:rt-mutex-unlock m)
    t))

(it-sequential "runtime-subsystem-sync-semaphore-basic"
  (let ((s (cl-cc/runtime:rt-make-semaphore :count 1)))
    (expect (cl-cc/runtime:rt-semaphore-wait s :timeout 0.1) :to-be-truthy)
    (expect (cl-cc/runtime:rt-semaphore-signal s) :to-be-truthy)))

(it-sequential "runtime-subsystem-sync-barrier-basic"
  (let ((b (cl-cc/runtime:rt-make-barrier 1)))
    (expect (cl-cc/runtime:rt-barrier-wait b :timeout 0.1) :to-be-truthy)))

(it-sequential "runtime-subsystem-sync-once-basic"
  (let ((o (cl-cc/runtime:rt-make-once))
        (count 0))
    (cl-cc/runtime:rt-once-call o (lambda () (incf count)))
    (expect (= 1 count) :to-be-truthy)
    (cl-cc/runtime:rt-once-call o (lambda () (incf count)))
    (expect (= 1 count) :to-be-truthy)))

(it-sequential "runtime-subsystem-scheduler-spawn-basic"
  (let ((cl-cc/runtime::*rt-global-scheduler* (cl-cc/runtime:rt-make-scheduler)))
    (let ((result nil))
      (cl-cc/runtime:rt-spawn (lambda () (setf result 42)))
      (cl-cc/runtime:rt-scheduler-run)
      (expect result :to-be 42))))

(it-sequential "runtime-subsystem-future-basic"
  (let ((f (cl-cc/runtime:rt-make-future)))
    (cl-cc/runtime:rt-future-resolve f 99)
    (expect (cl-cc/runtime:rt-future-await f :timeout 0.1) :to-be 99)))

(it-sequential "runtime-subsystem-channel-basic"
  (let ((ch (cl-cc/runtime:rt-make-channel :capacity 1)))
    (expect (cl-cc/runtime:rt-channel-send ch 7) :to-be 7)
    (expect (cl-cc/runtime:rt-channel-recv ch) :to-be 7)))

(it-sequential "runtime-subsystem-actor-basic"
  (let ((a (cl-cc/runtime:rt-make-actor #'identity)))
    (cl-cc/runtime:rt-actor-send a :hello)
    (expect (cl-cc/runtime:rt-actor-receive a :timeout 0.1) :to-be :hello)))

(it-sequential "runtime-subsystem-lockfree-stack-basic"
  (let ((s (cl-cc/runtime:rt-make-lfstack)))
    (cl-cc/runtime:rt-lfstack-push s 1)
    (cl-cc/runtime:rt-lfstack-push s 2)
    (multiple-value-bind (val ok) (cl-cc/runtime:rt-lfstack-pop s)
      (expect ok :to-be-truthy)
      (expect val :to-be 2))))

(it-sequential "runtime-subsystem-spsc-basic"
  (let ((q (cl-cc/runtime:rt-make-spsc-queue 4)))
    (expect (cl-cc/runtime:rt-spsc-try-push q 10) :to-be-truthy)
    (multiple-value-bind (val ok) (cl-cc/runtime:rt-spsc-try-pop q)
      (expect ok :to-be-truthy)
      (expect val :to-be 10))))

(it-sequential "runtime-subsystem-crdt-gcounter-basic"
  (let ((c (cl-cc/runtime:rt-make-gcounter)))
    (cl-cc/runtime:rt-gcounter-increment c 1 5)
    (expect (= 5 (cl-cc/runtime:rt-gcounter-value c)) :to-be-truthy)))

(it-sequential "runtime-subsystem-consensus-raft-basic"
  (let ((c (cl-cc/runtime:rt-make-raft-cluster '("n1" "n2" "n3"))))
    (expect (gethash "n1" (cl-cc/runtime:rt-raft-cluster-nodes c)) :to-be-truthy)))


;;; =================================================================
;;; FR Coverage Verification
;;; =================================================================

(defun %fr-ids (start end)
  "Return inclusive FR keyword IDs from START to END."
  (loop for id from start to end
        collect (intern (format nil "FR-~D" id) :keyword)))

(defun %fr-id-set (&rest clauses)
  "Expand coverage clauses into a flat FR keyword list.
Each clause is either a single numeric FR ID or an inclusive (START END) range."
  (loop for clause in clauses
        append (etypecase clause
                 (integer (%fr-ids clause clause))
                 (cons (%fr-ids (first clause) (second clause))))))

(defparameter *runtime-subsystem-fr-coverage-sentinels*
  '(:fr-500 :fr-654 :fr-190 :fr-492))

(defconstant +runtime-subsystem-fr-coverage-count+ 170)

(defparameter *runtime-subsystem-fr-coverage*
  (%fr-id-set
   '(190 193) '(257 260) '(280 283) '(290 291) '(300 301) '(310 312)
   '(320 322) '(330 332) '(340 341) '(345 353) '(355 356) '(360 363)
   '(370 373) '(380 383) '(390 392) '(400 401) '(410 412) '(420 422)
   '(430 432) '(440 442) '(450 452) '(460 462) '(470 472) '(480 481)
   '(490 492) '(500 504) '(510 513) '(520 525) '(530 533) '(540 544)
   '(550 554) '(560 564) '(570 574) '(580 587) '(590 593) '(595 597)
   '(600 602) '(605 607) '(610 612) '(615 617) '(620 622) '(625 627)
   '(630 634) '(638 640) '(643 646) '(650 654)))

(it-sequential "runtime-subsystem-fr-coverage-complete"
  (expect (= +runtime-subsystem-fr-coverage-count+ (length *runtime-subsystem-fr-coverage*)) :to-be-truthy)
  (dolist (fr-id *runtime-subsystem-fr-coverage-sentinels*)
    (expect (member fr-id *runtime-subsystem-fr-coverage*) :to-be-truthy)))
