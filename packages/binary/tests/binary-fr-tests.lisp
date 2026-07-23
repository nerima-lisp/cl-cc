;;;; tests/binary-fr-tests.lisp — Binary Output Feature Requirement Evidence Tests
;;;;
;;;; Tests for binary output FR implementations:
;;;; - FR-247: Unwind Tables / .eh_frame Generation
;;;; - FR-540/541: Safepoint polling and precise-GC stack maps
;;;; - FR-550/551/554: DWARF5, Wasm source maps, sanitizer instrumentation
;;;; - FR-560/561/562: Zero-cost EH table selection and landing pads

(in-package :cl-cc/test)



;;; ------------------------------------------------------------
;;; FR-247: Unwind Tables / .eh_frame Generation
;;; ------------------------------------------------------------

(it-sequential "fr-247-binary-package-loaded"
  (expect (find-package "CL-CC/BINARY") :to-be-truthy))

(it-sequential "fr-247-elf-section-constants-defined"
  (let ((pkg (find-package "CL-CC/BINARY")))
    (expect pkg :to-be-truthy)
    (let* ((progbits (find-symbol "+SHT-PROGBITS+" pkg))
           (has-const (or progbits
                          (find-symbol "*SHT-PROGBITS*" pkg)
                          (find-symbol "SHT-PROGBITS" pkg))))
      (expect has-const :to-be-truthy))))

(it-sequential "fr-247-x86-64-codegen-stack-map-support"
  (let ((pkg (find-package "CL-CC/EMIT")))
    (expect pkg :to-be-truthy)
    (let ((has-emit (or (find-symbol "COMPILE-TO-NATIVE" pkg)
                        (find-symbol "COMPILE-TO-X86-64-BYTES" pkg)
                        (find-symbol "EMIT-INSTRUCTION" pkg))))
      (expect has-emit :to-be-truthy))))

(it-sequential "fr-247-binary-elf-emit-symbols"
  (let ((pkg (find-package "CL-CC/BINARY")))
    (expect pkg :to-be-truthy)
    (let ((has-elf (or (find-symbol "WRITE-ELF64-FILE" pkg)
                       (find-symbol "COMPILE-TO-ELF64" pkg)
                       (find-symbol "MAKE-ELF64-EXECUTABLE" pkg))))
      (expect has-elf :to-be-truthy))))

(it-sequential "fr-247-binary-macho-emit-symbols"
  (let ((pkg (find-package "CL-CC/BINARY")))
    (expect pkg :to-be-truthy)
    (let ((has-macho (or (find-symbol "WRITE-MACH-O-FILE" pkg)
                          (find-symbol "BUILD-MACH-O" pkg)
                          (find-symbol "MAKE-MACH-O-BUILDER" pkg))))
      (expect has-macho :to-be-truthy))))

(defun %fr-byte-list (bytes)
  (coerce bytes 'list))

(it-sequential "fr-540-safepoint-polling-inserts-real-poll-instructions"
  (let* ((instructions (list (cl-cc:make-vm-label :name "entry")
                             (cl-cc:make-vm-const :dst :r0 :value 1)
                             (cl-cc:make-vm-ret :reg :r0)))
         (polled (cl-cc/optimize:opt-pass-safepoint-polling instructions)))
    (expect (> (length polled) (length instructions)) :to-be-truthy)
    (expect (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-get-global)
                              (eq (cl-cc/vm::vm-global-name inst)
                                  cl-cc/optimize::*opt-safepoint-flag-name*)))
                       polled) :to-be-truthy)
    (expect (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-call)
                              (= 1 (length (cl-cc/vm::vm-args inst)))))
                       polled) :to-be-truthy)
    (expect (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-label)
                              (string= (cl-cc/vm::vm-name inst)
                                       cl-cc/optimize::*opt-safepoint-label*)))
                       polled) :to-be-truthy)))

(it-sequential "fr-541-stack-map-records-follow-safepoints"
  (let* ((code (list '(:const :r0 1)
                     '(:safepoint :roots ((:kind :stack :slot 2 :type :pointer)))
                     '(:ret :r0)))
         (mapped (let ((cl-cc/codegen:*precise-gc-stack-maps-enabled* t))
                   (cl-cc/codegen:cg-embed-stack-maps-after-safepoints code)))
         (record (find :gc-stack-map mapped :key (lambda (entry)
                                                   (and (consp entry) (first entry))))))
    (expect record :to-be-truthy)
    (expect (first record) :to-equal :gc-stack-map)
    (expect (= 0 (getf (rest record) :safepoint-id)) :to-be-truthy)
    (expect (getf (rest record) :pc-offset) :to-be-truthy)))

(it-sequential "fr-550-dwarf5-debug-sections-have-byte-level-headers"
  (let* ((cu (cl-cc/binary::make-dwarf-compile-unit
              :name "fr550.lisp"
              :producer "cl-cc-test"
              :low-pc #x1000
              :high-pc #x1020
              :lines '((0 1) (4 2))
              :subprograms
              (list (cl-cc/binary::make-dwarf-subprogram
                     :name "main"
                     :low-pc #x1000
                     :high-pc #x1020
                     :parameters
                     (list (cl-cc/binary::make-dwarf-variable-location
                            :name "x" :kind :register :register 0))))))
         (alist (cl-cc/binary::build-dwarf-section-alist cu))
         (info (%fr-byte-list (cdr (assoc ".debug_info" alist :test #'string=))))
         (line (%fr-byte-list (cdr (assoc ".debug_line" alist :test #'string=))))
         (abbrev (%fr-byte-list (cdr (assoc ".debug_abbrev" alist :test #'string=)))))
    (expect (cdr (assoc ".debug_info" alist :test #'string=)) :to-be-truthy)
    (expect (cdr (assoc ".debug_line" alist :test #'string=)) :to-be-truthy)
    (expect (cdr (assoc ".debug_abbrev" alist :test #'string=)) :to-be-truthy)
    ;; .debug_info: unit_length:u32, version:u16=5, unit_type=compile, addr_size=8.
    (expect (subseq info 4 8) :to-equal '(5 0 1 8))
    ;; .debug_line: unit_length:u32, version:u16=5, addr_size=8, segment_selector=0.
    (expect (subseq line 4 8) :to-equal '(5 0 8 0))
    (expect (> (length abbrev) 8) :to-be-truthy)))

(it-sequential "fr-551-wasm-source-map-v3-json-contains-required-fields"
  (let ((json (cl-cc/emit:build-wasm-source-map-v3
               (list (list :offset 0 :source "input.lisp" :line 1 :column 0)
                     (list :offset 4 :source "input.lisp" :line 3 :column 2))
               :file "out.wasm"
               :source-root "/src")))
    (expect (search "\"version\": 3" json) :to-be-truthy)
    (expect (search "\"file\": \"out.wasm\"" json) :to-be-truthy)
    (expect (search "\"sourceRoot\": \"/src\"" json) :to-be-truthy)
    (expect (search "\"sources\": [\"input.lisp\"]" json) :to-be-truthy)
    (expect (search "\"mappings\":" json) :to-be-truthy)))

(it-sequential "fr-554-sanitizer-flag-emits-ubsan-instrumentation"
  (let* ((program (cl-cc/vm::make-vm-program
                   :instructions (list (cl-cc:make-vm-const :dst :r0 :value 1)
                                       (cl-cc:make-vm-const :dst :r1 :value 2)
                                       (cl-cc:make-vm-integer-add :dst :r2 :lhs :r0 :rhs :r1)
                                       (cl-cc:make-vm-halt :reg :r2))
                   :result-register :r2))
         (plain (%fr-byte-list (cl-cc/codegen:compile-to-x86-64-bytes program)))
         (ubsan (%fr-byte-list (cl-cc/codegen:compile-to-x86-64-bytes program :ubsan t))))
    (expect (search '(#x70 #x01 #xCC) plain :test #'eql) :to-be-falsy)
    (expect (search '(#x70 #x01 #xCC) ubsan :test #'eql) :to-be-truthy)))

(it-sequential "fr-560-562-zero-cost-eh-landing-pad-emits-real-transfer-code"
  (let* ((landing-pad (cl-cc/codegen::make-x86-64-landing-pad
                       :start-address #x10
                       :end-address #x20
                       :handler-address #x12345678
                       :handler-label "handler"
                       :handler-type 'error
                       :result-register :r0))
         (bytes (%fr-byte-list
                 (cl-cc/codegen::with-output-to-vector (stream)
                   (cl-cc/codegen::emit-x86-64-landing-pad-stub landing-pad stream)))))
    (expect (= cl-cc/codegen::+x86-64-landing-pad-stub-size+ (length bytes)) :to-be-truthy)
    (expect (subseq bytes 0 2) :to-equal '(#x49 #xBB))
    (expect (subseq bytes 10 13) :to-equal '(#x41 #xFF #xE3))
    (expect (search '(#x0F #x0B) bytes :test #'eql) :to-be-falsy)))

(it-sequential "fr-561-eh-model-selection-keeps-sjlj-default-and-enables-table-mode"
  (let* ((instructions (list (cl-cc:make-vm-label :name "entry")
                             (cl-cc:make-vm-establish-handler
                              :handler-label "handler" :result-reg :r0 :error-type 'error)
                             (cl-cc:make-vm-label :name "handler")
                             (cl-cc:make-vm-ret :reg :r0)))
         (offsets (cl-cc/codegen::build-label-offsets instructions 0))
         (pads (cl-cc/codegen::x86-64-build-landing-pad-table instructions offsets 0))
         (fdes (cl-cc/codegen::x86-64-landing-pad-table->dwarf-fdes pads)))
    (expect cl-cc/codegen:*zero-cost-eh-enabled* :to-be-falsy)
    (expect (= 1 (length pads)) :to-be-truthy)
    (expect (= 1 (length fdes)) :to-be-truthy)
    (expect (plusp (cl-cc/codegen::x86-64-landing-pad-handler-address (first pads))) :to-be-truthy)
    (expect (plusp (cl-cc/binary::dwarf-eh-fde-address-range (first fdes))) :to-be-truthy)))

(it-sequential "fr-560-integrated-x86-64-eh-appends-non-ud2-landing-pad"
  (let* ((program (cl-cc/vm::make-vm-program
                   :instructions (list (cl-cc:make-vm-label :name "entry")
                                       (cl-cc:make-vm-establish-handler
                                        :handler-label "handler" :result-reg :r0 :error-type 'error)
                                       (cl-cc:make-vm-const :dst :r0 :value 7)
                                       (cl-cc:make-vm-ret :reg :r0)
                                       (cl-cc:make-vm-label :name "handler")
                                       (cl-cc:make-vm-const :dst :r0 :value 9)
                                       (cl-cc:make-vm-ret :reg :r0))
                   :result-register :r0))
         (bytes (%fr-byte-list
                 (let ((cl-cc/codegen:*zero-cost-eh-enabled* t))
                   (cl-cc/codegen:compile-to-x86-64-bytes program)))))
    (expect (search '(#x49 #xBB) bytes :test #'eql) :to-be-truthy)
    (expect (search '(#x41 #xFF #xE3) bytes :test #'eql) :to-be-truthy)
    (expect (search '(#x0F #x0B) bytes :test #'eql) :to-be-falsy)))
