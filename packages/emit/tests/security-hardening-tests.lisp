;;;; packages/emit/tests/security-hardening-tests.lisp — security hardening byte/header evidence

(in-package :cl-cc/test)



(defun %security-byte-list (bytes)
  (coerce bytes 'list))

(defun %security-x86-bytes (thunk)
  (%security-byte-list
   (cl-cc/codegen::with-output-to-vector (out)
     (funcall thunk out))))

(defun %security-a64-bytes (thunk)
  (let ((bytes nil))
    (funcall thunk (lambda (byte) (push byte bytes)))
    (nreverse bytes)))

(defun %security-u16le (bytes offset)
  (+ (aref bytes offset)
     (ash (aref bytes (+ offset 1)) 8)))

(defun %security-u32le (bytes offset)
  (+ (aref bytes offset)
     (ash (aref bytes (+ offset 1)) 8)
     (ash (aref bytes (+ offset 2)) 16)
     (ash (aref bytes (+ offset 3)) 24)))

(defun %security-u64le (bytes offset)
  (loop for i below 8
        sum (ash (aref bytes (+ offset i)) (* 8 i))))

(defun %security-c-string (bytes offset)
  (with-output-to-string (out)
    (loop for i from offset below (length bytes)
          for byte = (aref bytes i)
          until (zerop byte)
          do (write-char (code-char byte) out))))

(defun %security-elf-section-names (bytes)
  (let* ((shoff (%security-u64le bytes 40))
         (shentsize (%security-u16le bytes 58))
         (shnum (%security-u16le bytes 60))
         (shstrndx (%security-u16le bytes 62))
         (shstr-header (+ shoff (* shstrndx shentsize)))
         (shstr-offset (%security-u64le bytes (+ shstr-header 24))))
    (loop for i below shnum
          for header = (+ shoff (* i shentsize))
          for name-offset = (%security-u32le bytes header)
          collect (%security-c-string bytes (+ shstr-offset name-offset)))))

(defun %security-indirect-call-program ()
  (cl-cc/vm::make-vm-program
   :instructions (list (cl-cc:make-vm-call :dst :R0 :func :R1 :args nil)
                       (cl-cc:make-vm-halt :reg :R0))
   :result-register :R0
   :leaf-p nil))

(defun %security-stack-buffer-program ()
  (cl-cc/vm::make-vm-program
   :instructions (list (cl-cc:make-vm-const :dst :R0 :value 4)
                       (cl-cc:make-vm-halt :reg :R0))
   :result-register :R0
   :leaf-p nil))

(it-sequential "fr-530-cfi-encoders-emit-endbr64-and-bti-c"
  (expect (%security-x86-bytes
                 (lambda (out)
                   (cl-cc/codegen::emit-x86-64-cfi-entry
                    out (cl-cc/codegen::x86-64-cfi-plan :has-indirect-calls-p t)))) :to-equal '(#xF3 #x0F #x1E #xFA))
  (expect (%security-a64-bytes
                 (lambda (out)
                   (cl-cc/codegen::emit-aarch64-cfi-entry
                    out (cl-cc/codegen::aarch64-cfi-plan :has-indirect-calls-p t)))) :to-equal '(#x5F #x24 #x03 #xD5)))

(it-sequential "fr-531-aarch64-pac-encoders-and-program-bytes"
  (expect (%security-a64-bytes
                 (lambda (out) (cl-cc/codegen::emit-a64-instr (cl-cc/codegen::encode-paciasp) out))) :to-equal '(#x3F #x23 #x03 #xD5))
  (expect (%security-a64-bytes
                 (lambda (out) (cl-cc/codegen::emit-a64-instr (cl-cc/codegen::encode-autiasp) out))) :to-equal '(#xBF #x23 #x03 #xD5))
  (let ((bytes (%security-byte-list
                (let ((cl-cc/codegen::*aarch64-pac-enabled* t))
                  (cl-cc/codegen::compile-to-aarch64-bytes
                   (%security-indirect-call-program))))))
    (expect (search '(#x3F #x23 #x03 #xD5) bytes :test #'eql) :to-be-truthy)
    (expect (search '(#xBF #x23 #x03 #xD5) bytes :test #'eql) :to-be-truthy)))

(it-sequential "fr-532-stack-protector-flag-materializes-canary-bytes"
  (let ((bytes (%security-byte-list
                (cl-cc/codegen::compile-to-x86-64-bytes
                 (%security-stack-buffer-program)
                 :stack-protector t))))
    (expect (search '(#x64 #x48 #x8B #x04 #x25 #x28 #x00 #x00 #x00) bytes :test #'eql) :to-be-truthy)
    (expect (search '(#x64 #x48 #x3B #x04 #x25 #x28 #x00 #x00 #x00) bytes :test #'eql) :to-be-truthy)
    (expect (search '(#x0F #x0B) bytes :test #'eql) :to-be-truthy)))

(it-sequential "fr-534-spectre-mitigations-emit-lfence-retpoline-bytes"
  (let ((bytes (%security-byte-list
                (cl-cc/codegen::compile-to-x86-64-bytes
                 (%security-indirect-call-program)
                 :spectre-mitigations t))))
    (expect (search '(#x0F #xAE #xE8) bytes :test #'eql) :to-be-truthy)
    (expect (search '(#xF3 #x90 #x0F #xAE #xE8) bytes :test #'eql) :to-be-truthy)))

(it-sequential "fr-651-elf-got-plt-relocation-entries-are-materialized"
  (let ((builder (cl-cc/binary::make-elf64-object)))
    (expect (cl-cc/binary::elf64-add-got-entry builder "puts") :to-equal 0)
    (expect (cl-cc/binary::elf64-add-plt-stub builder "puts") :to-equal 0)
    (expect (%security-byte-list (cl-cc/binary::binary-buffer-to-array
                                        (cl-cc/binary::elf64-text-buf builder))) :to-equal '(#xFF #x25 #x00 #x00 #x00 #x00 #x0F #x1F #x40 #x00))
    (let ((reloc (first (cl-cc/binary::elf64-rela-entries builder))))
      (expect (first reloc) :to-equal 2)
      (expect (second reloc) :to-equal cl-cc/binary::+r-x86-64-pc32+)
      (expect (third reloc) :to-equal "puts")
      (expect (fourth reloc) :to-equal -4))))

(it-sequential "fr-652-split-dwarf-dwo-sections-and-debuglink-are-well-formed"
  (let* ((dwo (cl-cc/binary::build-dwo-file "unit.dwo"))
         (names (%security-elf-section-names dwo))
         (skeleton (cl-cc/binary::build-dwarf-skeleton-cu "unit.dwo"))
         (debuglink (cl-cc/binary::build-gnu-debuglink-section "unit.dwo" dwo)))
    (expect (member ".debug_info.dwo" names :test #'string=) :to-be-truthy)
    (expect (member ".debug_abbrev.dwo" names :test #'string=) :to-be-truthy)
    (expect (member ".debug_line.dwo" names :test #'string=) :to-be-truthy)
    (expect (search '(#x30 #x21) (%security-byte-list skeleton) :test #'eql) :to-be-truthy)
    (expect (%security-c-string debuglink 0) :to-equal "unit.dwo")
    (expect (mod (- (length debuglink) 4) 4) :to-equal 0)))

(it-sequential "fr-771-safestack-dual-stack-hooks-are-in-full-program-output"
  (let ((x86 (%security-byte-list
              (let ((cl-cc/codegen::*x86-64-safe-stack-enabled* t))
                (cl-cc/codegen::compile-to-x86-64-bytes
                 (cl-cc/vm::make-vm-program
                  :instructions (list (cl-cc:make-vm-const :dst :R0 :value 7)
                                      (cl-cc:make-vm-halt :reg :R0))
                  :result-register :R0
                  :leaf-p nil))))))
    (expect (search '(#x64 #x4C #x8B #x1C #x25 #x70 #x00 #x00 #x00) x86 :test #'eql) :to-be-truthy)
    (expect (search '(#x64 #x4C #x89 #x1C #x25 #x70 #x00 #x00 #x00) x86 :test #'eql) :to-be-truthy))
  (let ((a64 (%security-byte-list
              (let ((cl-cc/codegen::*aarch64-safe-stack-enabled* t))
                (cl-cc/codegen::compile-to-aarch64-bytes
                 (cl-cc/vm::make-vm-program
                  :instructions (list (cl-cc:make-vm-const :dst :R0 :value 7)
                                      (cl-cc:make-vm-halt :reg :R0))
                  :result-register :R0
                  :leaf-p nil))))))
    ;; Both load and store paths begin by reading TPIDR_EL0 through MRS x16.
    (expect (search (%security-a64-bytes
                          (lambda (out)
                            (cl-cc/codegen::emit-a64-safe-stack-load-pointer
                             cl-cc/codegen::+a64-scs-tmp+ out)))
                         a64 :test #'eql) :to-be-truthy)
    (expect (search (%security-a64-bytes
                          (lambda (out)
                            (cl-cc/codegen::emit-a64-safe-stack-store-pointer
                             cl-cc/codegen::+a64-scs-tmp+ out)))
                         a64 :test #'eql) :to-be-truthy)))

(it-sequential "fr-772-runtime-xom-effective-protection-removes-read-when-supported"
  (let ((cl-cc/runtime::*xom-enabled* nil))
    (expect (cl-cc/runtime::rt-xom-effective-prot) :to-equal (logior cl-cc/runtime::+rt-prot-read+ cl-cc/runtime::+rt-prot-exec+)))
  (let ((cl-cc/runtime::*xom-enabled* t))
    (if (cl-cc/runtime::rt-xom-supported-p)
        (expect (cl-cc/runtime::rt-xom-effective-prot) :to-equal cl-cc/runtime::+rt-prot-exec+)
        (expect (cl-cc/runtime::rt-xom-effective-prot) :to-equal (logior cl-cc/runtime::+rt-prot-read+ cl-cc/runtime::+rt-prot-exec+)))))
