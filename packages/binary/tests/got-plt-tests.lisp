;;;; packages/binary/tests/got-plt-tests.lisp — GOT/PLT section generation tests
;;;;
;;;; Tests for: add-plt-stubs, add-got-entries, add-dynamic-relocations,
;;;; bind-now-mode, setup-got-plt (got-plt.lisp).
;;;;
;;;; PLT layout (System V AMD64 ABI):
;;;;   PLT[0]  16 bytes: resolver stub  (ff 35 ... ff 25 ...)
;;;;   PLT[n]  16 bytes each: jmpq *GOT[n+3](%rip); push $n; jmpq PLT[0]
;;;;
;;;; GOT layout (.got.plt):
;;;;   GOT[0..2]  reserved (3 × 8 bytes = 24 bytes)
;;;;   GOT[3+i]   per-symbol slot (8 bytes each), filled lazily by ld.so

(in-package :cl-cc/test)



;;; ─── bind-now-mode ───────────────────────────────────────────────────────

(it-sequential "got-plt-bind-now-mode-returns-df-bind-now"
  (expect (cl-cc/binary::bind-now-mode) :to-equal cl-cc/binary::+df-bind-now+))

;;; ─── add-plt-stubs ───────────────────────────────────────────────────────

(it-sequential "got-plt-add-plt-stubs-empty-returns-16-bytes"
  (let ((plt (cl-cc/binary::add-plt-stubs '())))
    (expect (length plt) :to-equal 16)))

(it-sequential "got-plt-add-plt-stubs-one-symbol-returns-32-bytes"
  (let ((plt (cl-cc/binary::add-plt-stubs '("foo"))))
    (expect (length plt) :to-equal 32)))

(it-sequential "got-plt-add-plt-stubs-n-symbols-correct-size"
  (dolist (n '(0 1 2 5 10))
    (let ((syms (loop repeat n collect "sym")))
      (expect (length (cl-cc/binary::add-plt-stubs syms)) :to-equal (* 16 (1+ n))))))

(it-sequential "got-plt-add-plt-stubs-plt0-resolver-opcodes"
  (let* ((plt (cl-cc/binary::add-plt-stubs '()))
         (b0 (aref plt 0))
         (b1 (aref plt 1)))
    (expect b0 :to-equal #xff)
    (expect b1 :to-equal #x35)))

(it-sequential "got-plt-add-plt-stubs-entry-jmpq-opcode"
  (let* ((plt (cl-cc/binary::add-plt-stubs '("foo")))
         (b16 (aref plt 16))
         (b17 (aref plt 17)))
    (expect b16 :to-equal #xff)
    (expect b17 :to-equal #x25)))

(it-sequential "got-plt-add-plt-stubs-entry-push-index"
  (let* ((plt (cl-cc/binary::add-plt-stubs '("foo")))
         ;; push $0: opcode 68, then 4-byte little-endian index 0
         (push-opcode (aref plt 22))
         (index-lo    (aref plt 23)))
    (expect push-opcode :to-equal #x68)
    (expect index-lo :to-equal 0)))

(it-sequential "got-plt-add-plt-stubs-second-entry-push-index-1"
  (let* ((plt (cl-cc/binary::add-plt-stubs '("foo" "bar")))
         (push-opcode (aref plt 38))   ; 16 + 16 + 6 = 38
         (index-lo    (aref plt 39)))
    (expect push-opcode :to-equal #x68)
    (expect index-lo :to-equal 1)))

;;; ─── add-got-entries ─────────────────────────────────────────────────────

(it-sequential "got-plt-add-got-entries-zero-symbols-has-3-reserved"
  (let ((got (cl-cc/binary::add-got-entries 0)))
    (expect (length got) :to-equal 24)))

(it-sequential "got-plt-add-got-entries-n-symbols-correct-size"
  (dolist (n '(0 1 2 5 10))
    (expect (length (cl-cc/binary::add-got-entries n)) :to-equal (* 8 (+ 3 n)))))

(it-sequential "got-plt-add-got-entries-all-zeros"
  (let ((got (cl-cc/binary::add-got-entries 2)))
    (expect (every #'zerop got) :to-be-truthy)))

;;; ─── add-dynamic-relocations ─────────────────────────────────────────────

(it-sequential "got-plt-add-dynamic-relocations-empty-returns-empty"
  (let ((rela (cl-cc/binary::add-dynamic-relocations '() 0)))
    (expect (length rela) :to-equal 0)))

(it-sequential "got-plt-add-dynamic-relocations-one-symbol-24-bytes"
  (let ((rela (cl-cc/binary::add-dynamic-relocations '("foo") 0)))
    (expect (length rela) :to-equal 24)))

(it-sequential "got-plt-add-dynamic-relocations-n-symbols-correct-size"
  (dolist (n '(1 2 5))
    (let ((syms (loop repeat n collect "sym")))
      (expect (length (cl-cc/binary::add-dynamic-relocations syms 0)) :to-equal (* 24 n)))))

(it-sequential "got-plt-add-dynamic-relocations-r-info-type-is-jump-slot"
  (let* ((rela (cl-cc/binary::add-dynamic-relocations '("foo") 0))
         ;; Low 32 bits of r_info: bytes 8-11
         (r-type (logior (aref rela 8)
                         (ash (aref rela 9)  8)
                         (ash (aref rela 10) 16)
                         (ash (aref rela 11) 24))))
    (expect r-type :to-equal cl-cc/binary::+r-x86-64-jump-slot+)))

(it-sequential "got-plt-add-dynamic-relocations-r-offset-targets-got3"
  (let* ((got-base #x1000)
         (rela (cl-cc/binary::add-dynamic-relocations '("foo") got-base))
         ;; r_offset: bytes 0-7 of first entry (little-endian u64)
         (r-offset (logior (aref rela 0)
                           (ash (aref rela 1) 8)
                           (ash (aref rela 2) 16)
                           (ash (aref rela 3) 24))))
    ;; GOT[3] = got-base + 3*8 = #x1018
    (expect r-offset :to-equal (+ got-base 24))))

;;; ─── setup-got-plt (integration) ────────────────────────────────────────

(it-sequential "got-plt-setup-returns-four-values"
  (multiple-value-bind (plt got rela bind-now)
      (cl-cc/binary::setup-got-plt '("foo" "bar"))
    (expect (typep plt  '(simple-array (unsigned-byte 8) (*))) :to-be-truthy)
    (expect (typep got  '(simple-array (unsigned-byte 8) (*))) :to-be-truthy)
    (expect (typep rela '(simple-array (unsigned-byte 8) (*))) :to-be-truthy)
    (expect bind-now :to-be-truthy)))

(it-sequential "got-plt-setup-sizes-are-consistent"
  (let ((syms '("malloc" "free" "puts")))
    (multiple-value-bind (plt got rela _)
        (cl-cc/binary::setup-got-plt syms)
      (declare (ignore _))
      (expect (length plt) :to-equal (* 16 (1+ (length syms))))
      (expect (length got) :to-equal (* 8  (+ 3 (length syms))))
      (expect (length rela) :to-equal (* 24 (length syms))))))
