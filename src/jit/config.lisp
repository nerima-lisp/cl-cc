;;;; src/jit/config.lisp — JIT feature gates

(in-package :cl-cc/jit)

(defvar *jit-enabled* nil
  "When T, high-level JIT compilation entry points may run.")

(defvar *jit-native-code-enabled* nil
  "When T, the JIT may allocate executable/native memory and patch stubs.")

(defvar *jit-cache-enabled* nil
  "When T, the JIT may read and write persistent code cache files.")

(defun ensure-jit-native-code-enabled (operation)
  "Signal an error unless native code generation is explicitly enabled."
  (unless *jit-native-code-enabled*
    (error "JIT native code is disabled; cannot ~A." operation))
  t)

(defun sb-posix-map-anonymous-flag ()
  "Return the platform-specific SB-POSIX anonymous mmap flag."
  (flet ((maybe-flag (name)
           (multiple-value-bind (symbol status) (find-symbol name :sb-posix)
             (when (and status (boundp symbol))
               (symbol-value symbol)))))
    (or (maybe-flag "MAP-ANONYMOUS")
        (maybe-flag "MAP-ANON")
        (error "SB-POSIX does not expose MAP-ANONYMOUS or MAP-ANON."))))

(progn
  (sb-alien:define-alien-routine ("mprotect" %system-mprotect) sb-alien:int
    (address sb-sys:system-area-pointer)
    (length sb-alien:unsigned-long)
    (protection sb-alien:int))

  (defun sb-posix-mprotect (address length protection)
    "Protect a memory range through SB-POSIX or the host mprotect(2)."
    (multiple-value-bind (symbol status) (find-symbol "MPROTECT" :sb-posix)
      (if (and status (fboundp symbol))
          (funcall (symbol-function symbol) address length protection)
          (let ((result (%system-mprotect address length protection)))
            (unless (zerop result)
              (error "mprotect failed for ~D bytes with protection ~D."
                     length protection))
            result)))))
