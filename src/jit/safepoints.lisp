;;;; src/jit/safepoints.lisp — FR-551 Safepoints (セーフポイント)
;;;; Polling-based safe points for JIT-compiled code.
;;;; JVM Safepoint / HotSpot polling page / V8 safepoints equivalent.
(in-package :cl-cc/jit)

;;; ──── Configuration ────
(defvar *safepoint-enabled* t
  "When T, safepoint polling is inserted into JIT code.")

(defvar *safepoint-interval* 4096
  "Insert a safepoint poll every N bytes of generated code.")

(defvar *safepoint-flag* (sb-sys:int-sap 0)
  "Address of the safepoint flag page. When the GC sets this page to
non-readable, the next poll triggers a SEGV caught by the signal handler.")

(defvar *safepoint-lock* (sb-thread:make-mutex :name "cl-cc JIT safepoint coordination")
  "Lock protecting safepoint coordination.")

(defvar *safepoint-page-address* nil
    "Base address of the safepoint polling page.")
  (defvar *safepoint-armed-p* nil
    "True while polls are armed by making the page unreadable.")

;;; ──── Safepoint poll instruction ────
(defvar *bytes-since-last-safepoint* 0
  "Counter: bytes emitted since last safepoint poll was inserted.")

;;; ──── Safepoint page setup ────
(defun emit-safepoint-poll (stream)
  "Emit a polling instruction when supported; otherwise fail rather than emitting invalid code."
  (declare (ignore stream))
  (when (and *safepoint-enabled* (>= *bytes-since-last-safepoint* *safepoint-interval*))
    (error
      "JIT safepoint poll emission is unsupported without a code-address relocation facility")))

(defun init-safepoint-page ()
  "Allocate the normally readable safepoint polling page."
  (ensure-jit-native-code-enabled "initialize a safepoint page")
  (sb-thread:with-mutex
    (*safepoint-lock*)
    (when *safepoint-page-address*
      (error "Safepoint page is already initialized"))
    (let ((page
          (sb-posix:mmap
            nil
            4096
            sb-posix:prot-read
            (logior sb-posix:map-private (sb-posix-map-anonymous-flag))
            -1
            0)))
      (setf *safepoint-page-address* (sb-sys:sap-int page)
            *safepoint-armed-p* nil)
      page)))

(defun arm-safepoint ()
  "Arm safepoint polls by making the polling page inaccessible."
  (sb-thread:with-mutex
    (*safepoint-lock*)
    (unless *safepoint-page-address*
      (error "Safepoint page is not initialized"))
    (sb-posix-mprotect
      (sb-sys:int-sap *safepoint-page-address*)
      4096
      sb-posix:prot-none)
    (setf *safepoint-armed-p* t)
    (values)))

;;; ──── Stop-the-World implementation ────
(defun disarm-safepoint ()
  "Disarm safepoint polls by restoring read access to the polling page."
  (sb-thread:with-mutex
    (*safepoint-lock*)
    (unless *safepoint-page-address*
      (error "Safepoint page is not initialized"))
    (sb-posix-mprotect
      (sb-sys:int-sap *safepoint-page-address*)
      4096
      sb-posix:prot-read)
    (setf *safepoint-armed-p* nil)
    (values)))

(defvar *stw-in-progress* nil
  "T when a Stop-the-World pause is active.")

(defvar *thread-count-at-safepoint* 0
  "Number of threads that have reached the safepoint.")

;;; ──── Atomic increment helper ────
(defmacro atomic-incf (place)
  "Increment PLACE under the safepoint coordination lock."
  `(sb-thread:with-mutex (*safepoint-lock*) (incf ,place)))

(defun enter-safepoint ()
  "Called by a thread when it hits a safepoint poll during STW."
  (when *stw-in-progress*
    (atomic-incf *thread-count-at-safepoint*)
    ;; Spin until STW is complete (simplified — real impl uses futex)
    (loop while *stw-in-progress*
          do (sb-thread:thread-yield))))

;;; ──── Scope macro ────
(defmacro with-safepoints (&body body)
  "Execute BODY with safepoint polling enabled for JIT code generation."
  `(let ((*safepoint-enabled* t)
        (*bytes-since-last-safepoint* 0))
    ,@body))
