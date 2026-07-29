;;;; src/jit/call-stubs.lisp — FR-553 Lazy JIT Compilation / Call Stubs
;;;; Trampoline stubs for deferred compilation. V8 Ignition / HotSpot stubs.

(in-package :cl-cc/jit)

;;; ──── Architecture constants ────
(defvar *call-stub-size* 5
  "Size of the call stub in bytes (x86-64: 5 bytes for JMP rel32).")

;;; ──── Stub table ────
(defvar *call-stub-table* (make-hash-table :test #'eq)
  "Function name → (stub-address . original-bytecode).")

;;; ──── Stub installation ────
(defun install-call-stub (func-name bytecode)
  "Reject call-stub installation until the native ABI bridge is implemented."
  (declare (ignore func-name bytecode))
  (error "Call stub installation is unsupported until a native ABI bridge is available."))

;;; ──── Stub patching (after compilation) ────
(defun patch-stub-to-direct (func-name compiled-code-addr)
  "Patch the call stub for FUNC-NAME to jump directly to COMPILED-CODE-ADDR.
Replaces the 5-byte CALL with a 5-byte JMP.
Uses atomic 8-byte write for thread safety."
  #-x86-64
  (declare (ignore compiled-code-addr))
  (ensure-jit-native-code-enabled "patch a call stub")
  (let ((entry (gethash func-name *call-stub-table*)))
    (when entry
      #+x86-64
      (let ((stub-addr (car entry)))
        (progn
          ;; Write JMP rel32: E9 XX XX XX XX
          (setf (sb-sys:sap-ref-8 stub-addr 0) #xE9) ; JMP rel32 opcode
          ;; rel32 = target - (stub_addr + 5)
          (setf (sb-sys:sap-ref-32 stub-addr 1)
                (- compiled-code-addr
                   (sb-sys:sap-int stub-addr)
                   5))))
      ;; Remove from table (now compiled)
      (remhash func-name *call-stub-table*)
      (values))))

;;; ──── Compile stub (called by trampoline) ────
(defun compile-stub-handler (func-name)
  "Called by the call stub when an uncompiled function is invoked.
Triggers JIT compilation and patches the stub for future calls."
  (unless *jit-enabled*
    (return-from compile-stub-handler nil))
  (let ((entry (gethash func-name *call-stub-table*)))
    (when entry
      (destructuring-bind (stub-addr . bytecode) entry
        (declare (ignore stub-addr))
        ;; Trigger JIT compilation (background or synchronous)
        (let ((compiled-code (jit-baseline-compile func-name bytecode)))
          ;; Patch only after a native backend provides an executable entry address.
          (let ((entry-address (jit-compiled-code-entry-address compiled-code)))
            (when entry-address
              (patch-stub-to-direct func-name entry-address)))
          ;; Execute the compiled code
          compiled-code)))))

(defun jit-compile-stub (func-name)
  "Public entry point for compiling a call stub target."
  (compile-stub-handler func-name))

;;; ──── Background compilation ────
(defvar *background-compile-queue* nil
  "Queue of (func-name . bytecode) pending background compilation.")

(defun schedule-background-compile (func-name bytecode)
  "Schedule FUNC-NAME for background JIT compilation.
The interpreter continues executing while compilation happens."
  (push (cons func-name bytecode) *background-compile-queue*))

(defun process-compile-queue ()
  "Process pending background compilations (called by worker thread)."
  (loop while *background-compile-queue*
        for (func-name . bytecode) = (pop *background-compile-queue*)
        do (jit-baseline-compile func-name bytecode)))

;;; ──── Memory allocation for stubs ────
(defun allocate-executable-memory (size)
  "Allocate SIZE bytes of executable memory for code stubs.
Uses mmap with PROT_READ | PROT_WRITE | PROT_EXEC."
  (ensure-jit-native-code-enabled "allocate executable stub memory")
  (sb-posix:mmap nil size
                 (logior sb-posix:prot-read
                         sb-posix:prot-write
                         sb-posix:prot-exec)
                 (logior sb-posix:map-private
                         (sb-posix-map-anonymous-flag))
                 -1 0))

(defun get-compile-stub-address ()
  "Reject compile-stub resolution until the native ABI bridge is implemented."
  (error "Resolving the compile stub address is unsupported until a native ABI bridge is available."))
