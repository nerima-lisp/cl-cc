;;;; tests/unit/emit/macho-builder-tests.lisp — Mach-O Builder API and Extended Tests
;;;;
;;;; Tests for src/emit/binary/macho.lisp and src/emit/binary/macho-serialize.lisp:
;;;; Builder API, additional structure defaults, extended serialization, and binary output.

(in-package :cl-cc/test)


;;; ─── Builder API ────────────────────────────────────────────────────────

(it-sequential "macho-builder-creation x86-64"
  (destructuring-bind (target) (list :x86-64)
    (expect (cl-cc/binary:make-mach-o-builder target) :to-be-truthy)))

(it-sequential "macho-builder-creation arm64"
  (destructuring-bind (target) (list :arm64)
    (expect (cl-cc/binary:make-mach-o-builder target) :to-be-truthy)))

(it-sequential "macho-add-entry-point-succeeds"
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64)))
    (cl-cc/binary:add-entry-point b 0)
    (expect b :to-be-truthy)))

(it-sequential "macho-add-text-segment-appends-to-segments"
  (let* ((b (cl-cc/binary:make-mach-o-builder :x86-64))
         (code (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#xC3 0 0 0))))
    (cl-cc/binary:add-text-segment b code)
    (expect (length (cl-cc/binary::mach-o-builder-segments b)) :to-equal 1)))


(it-sequential "macho-data-segment-is-not-executable"
  (let* ((b (cl-cc/binary:make-mach-o-builder :x86-64))
         (data (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4))))
    (cl-cc/binary:add-data-segment b data)
    (let ((seg (find "__DATA" (cl-cc/binary::mach-o-builder-segments b)
                     :key #'cl-cc/binary:segment-command-segname :test #'string=)))
      (expect (null seg) :to-be-falsy)
      (expect (cl-cc/binary:segment-command-maxprot seg) :to-equal 6)
      (expect (cl-cc/binary:segment-command-initprot seg) :to-equal 6))))

(it-sequential "macho-build-binary-is-nonempty-ub8-vector-at-least-4096-bytes"
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64))
        (code (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#xC3 0 0 0))))
    (cl-cc/binary:add-entry-point b 0)
    (let ((result (cl-cc/binary:build-mach-o b code)))
      (expect (> (length result) 0) :to-be-truthy)
      (expect (typep result '(simple-array (unsigned-byte 8) (*))) :to-be-truthy)
      (expect (>= (length result) 4096) :to-be-truthy))))

(it-sequential "macho-build-binary-starts-with-feedfacf-magic"
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64))
        (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary:add-entry-point b 0)
    (let ((result (cl-cc/binary:build-mach-o b code)))
      (expect (aref result 0) :to-equal #xCF)
      (expect (aref result 1) :to-equal #xFA)
      (expect (aref result 2) :to-equal #xED)
      (expect (aref result 3) :to-equal #xFE))))


(it-sequential "macho-build-serializes-data-segment-payload"
  (let* ((b (cl-cc/binary:make-mach-o-builder :x86-64))
         (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3)))
         (data (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4))))
    (cl-cc/binary:add-text-segment b code)
    (cl-cc/binary:add-data-segment b data)
    (cl-cc/binary:add-entry-point b 0)
    (let* ((result (cl-cc/binary:build-mach-o b code))
           (data-seg (find "__DATA" (cl-cc/binary::mach-o-builder-segments b)
                           :key #'cl-cc/binary:segment-command-segname :test #'string=))
           (off (cl-cc/binary:segment-command-fileoff data-seg)))
      (expect (cl-cc/binary:segment-command-filesize data-seg) :to-equal 4)
      (expect (coerce (subseq result off (+ off 4)) 'list) :to-equal '(1 2 3 4)))))

(it-sequential "macho-build-serializes-symbol-table"
  (let* ((b (cl-cc/binary:make-mach-o-builder :x86-64))
         (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary:add-text-segment b code)
    (cl-cc/binary:add-symbol b "_main" :value 0 :sect 1)
    (cl-cc/binary:add-entry-point b 0)
    (let* ((result (cl-cc/binary:build-mach-o b code))
           (symtab-cmd-off (+ 32 72 232 152 72 32)))
      (expect (aref result 16) :to-equal 9)
      (expect (aref result symtab-cmd-off) :to-equal 12))))

(it-sequential "macho-build-x86-64-includes-pagezero-segment"
  (let ((b (cl-cc/binary:make-mach-o-builder :x86-64))
        (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary:add-text-segment b code)
    (cl-cc/binary:add-entry-point b 0)
    (let ((result (cl-cc/binary:build-mach-o b code)))
      (expect (aref result 16) :to-equal 7)
      (expect (aref result 17) :to-equal 0)
      (expect (aref result 18) :to-equal 0)
      (expect (aref result 19) :to-equal 0)
      (let ((pagezero (map 'string #'code-char (subseq result 40 50))))
        (expect pagezero :to-equal "__PAGEZERO")))))

(it-sequential "macho-build-arm64-has-different-cputype-from-x86-64"
  (let ((b (cl-cc/binary:make-mach-o-builder :arm64))
        (code (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#xD6 #x5F #x03 #xC0))))
    (cl-cc/binary:add-entry-point b 0)
    (let ((result (cl-cc/binary:build-mach-o b code)))
      (expect (aref result 0) :to-equal #xCF)
      (let ((b2 (cl-cc/binary:make-mach-o-builder :x86-64)))
        (cl-cc/binary:add-entry-point b2 0)
        (let ((x64 (cl-cc/binary:build-mach-o b2 code)))
          (expect (= (aref result 4) (aref x64 4)) :to-be-falsy))))))

;;; ─── Additional Structure Tests ─────────────────────────────────────────

(it-sequential "macho-structure-defaults"
  (let ((nl (cl-cc/binary::make-nlist)))
    (expect (cl-cc/binary::nlist-n-strx nl) :to-equal 0)
    (expect (cl-cc/binary::nlist-n-type nl) :to-equal 0)
    (expect (cl-cc/binary::nlist-n-sect nl) :to-equal 0)
    (expect (cl-cc/binary::nlist-n-desc nl) :to-equal 0)
    (expect (cl-cc/binary::nlist-n-value nl) :to-equal 0))
  (let ((sc (cl-cc/binary::make-symtab-command)))
    (expect (cl-cc/binary::symtab-command-cmd sc) :to-equal cl-cc/binary:+lc-symtab+)
    (expect (cl-cc/binary::symtab-command-cmdsize sc) :to-equal 24)
    (expect (cl-cc/binary::symtab-command-nsyms sc) :to-equal 0))
  (let ((sect (cl-cc/binary::make-section)))
    (expect (cl-cc/binary:section-sectname sect) :to-equal "")
    (expect (cl-cc/binary:section-segname sect) :to-equal "")
    (expect (cl-cc/binary:section-size sect) :to-equal 0)
    (expect (cl-cc/binary:section-addr sect) :to-equal 0)))

(it-sequential "macho-lc-main-constant-has-req-dyld-bit"
  (expect cl-cc/binary:+lc-main+ :to-equal #x80000028))

(it-sequential "macho-header-flag-and-cpu-subtype-constants"
  (expect cl-cc/binary:+mh-noundefs+ :to-equal 1)
  (expect cl-cc/binary:+mh-dyldlink+ :to-equal 4)
  (expect cl-cc/binary:+mh-pie+ :to-equal #x200000)
  (expect cl-cc/binary:+cpu-subtype-x86-64-all+ :to-equal #x00000003)
  (expect cl-cc/binary:+cpu-subtype-arm64-all+ :to-equal #x00000000))

;;; ─── Additional Serialization Tests ─────────────────────────────────────

(it-sequential "macho-serialize-nlist-produces-18-bytes"
  (let ((buf (cl-cc/binary::make-byte-buffer))
        (nl (cl-cc/binary::make-nlist :n-strx 1 :n-type #x0f :n-sect 1 :n-desc 0 :n-value 0)))
    (cl-cc/binary::serialize-nlist nl buf)
    (expect (length (cl-cc/binary::byte-buffer-data buf)) :to-equal 18)))

(it-sequential "macho-serialize-nlist-strx-in-little-endian"
  (let ((buf (cl-cc/binary::make-byte-buffer))
        (nl (cl-cc/binary::make-nlist :n-strx #x00000005)))
    (cl-cc/binary::serialize-nlist nl buf)
    (let ((data (cl-cc/binary::byte-buffer-data buf)))
      (expect (aref data 0) :to-equal 5)
      (expect (aref data 1) :to-equal 0))))

(it-sequential "macho-command-serialization-sizes entry-point"
  (destructuring-bind (expected obj serializer) (list 24 (cl-cc/binary::make-entry-point-command) #'cl-cc/binary::serialize-entry-point)
    (let ((buf (cl-cc/binary::make-byte-buffer)))
    (funcall serializer obj buf)
    (expect (length (cl-cc/binary::byte-buffer-data buf)) :to-equal expected))))

(it-sequential "macho-command-serialization-sizes symtab"
  (destructuring-bind (expected obj serializer) (list 24 (cl-cc/binary::make-symtab-command) #'cl-cc/binary::serialize-symtab-command)
    (let ((buf (cl-cc/binary::make-byte-buffer)))
    (funcall serializer obj buf)
    (expect (length (cl-cc/binary::byte-buffer-data buf)) :to-equal expected))))

(it-sequential "macho-command-serialization-sizes section"
  (destructuring-bind (expected obj serializer) (list 80 (cl-cc/binary::make-section :sectname "__text" :segname "__TEXT") #'cl-cc/binary::serialize-section)
    (let ((buf (cl-cc/binary::make-byte-buffer)))
    (funcall serializer obj buf)
    (expect (length (cl-cc/binary::byte-buffer-data buf)) :to-equal expected))))

(it-sequential "macho-buffer-write-bytes-appends-correctly"
  (let ((buf (cl-cc/binary::make-byte-buffer))
        (bytes (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(10 20 30))))
    (cl-cc/binary::buffer-write-bytes buf bytes)
    (let ((data (cl-cc/binary::byte-buffer-data buf)))
      (expect (length data) :to-equal 3)
      (expect (aref data 0) :to-equal 10)
      (expect (aref data 1) :to-equal 20)
      (expect (aref data 2) :to-equal 30))))

(it-sequential "macho-binary-buffer-writes-little-endian-u16-and-u8"
  (let ((buf (cl-cc/binary::make-binary-buffer 0)))
    (cl-cc/binary::binary-buffer-write-u16le buf #x1234)
    (cl-cc/binary::binary-buffer-write-u8 buf #x56)
    (expect (coerce (cl-cc/binary::binary-buffer-to-array buf) 'list) :to-equal '(#x34 #x12 #x56))))

;;; ─── Additional Builder API Tests ───────────────────────────────────────

(it-sequential "macho-builder-cputypes x86-64"
  (destructuring-bind (target expected-cputype) (list :x86-64 cl-cc/binary:+cpu-type-x86-64+)
    (let* ((b (cl-cc/binary:make-mach-o-builder target))
         (hdr (cl-cc/binary::mach-o-builder-header b)))
    (expect (cl-cc/binary:mach-header-cputype hdr) :to-equal expected-cputype))))

(it-sequential "macho-builder-cputypes arm64"
  (destructuring-bind (target expected-cputype) (list :arm64 cl-cc/binary:+cpu-type-arm64+)
    (let* ((b (cl-cc/binary:make-mach-o-builder target))
         (hdr (cl-cc/binary::mach-o-builder-header b)))
    (expect (cl-cc/binary:mach-header-cputype hdr) :to-equal expected-cputype))))

(it-sequential "macho-add-symbol-behavior one-symbol"
  (destructuring-bind (expected names) (list 1 '("_main"))
    (let ((b (cl-cc/binary:make-mach-o-builder :x86-64)))
    (dolist (name names)
      (cl-cc/binary:add-symbol b name :value 0 :sect 1))
    (expect (length (cl-cc/binary::mach-o-builder-symbol-table b)) :to-equal expected))))

(it-sequential "macho-add-symbol-behavior two-symbols"
  (destructuring-bind (expected names) (list 2 '("_start" "_exit"))
    (let ((b (cl-cc/binary:make-mach-o-builder :x86-64)))
    (dolist (name names)
      (cl-cc/binary:add-symbol b name :value 0 :sect 1))
    (expect (length (cl-cc/binary::mach-o-builder-symbol-table b)) :to-equal expected))))

;;; ─── Bug-fix regression: LC_LOAD_DYLINKER, MH_DYLDLINK, entryoff ───────

(it-sequential "macho-build-has-lc-load-dylinker"
  (let* ((b (cl-cc/binary:make-mach-o-builder :x86-64))
         (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary:add-text-segment b code)
    (cl-cc/binary:add-entry-point b 0)
    (let* ((result (cl-cc/binary:build-mach-o b code))
           (idx (position #x0E result)))
      (expect idx :to-be-truthy)
      (expect (aref result (+ idx 1)) :to-equal #x00)
      (expect (aref result (+ idx 2)) :to-equal #x00)
      (expect (aref result (+ idx 3)) :to-equal #x00))))

(it-sequential "macho-build-header-has-dyldlink-flag"
  (let* ((b (cl-cc/binary:make-mach-o-builder :x86-64))
         (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary:add-entry-point b 0)
    (let* ((result (cl-cc/binary:build-mach-o b code))
           (flags (+ (aref result 24)
                     (ash (aref result 25) 8)
                     (ash (aref result 26) 16)
                     (ash (aref result 27) 24))))
      (expect (logbitp 2 flags) :to-be-truthy))))

(it-sequential "macho-entryoff-is-code-offset"
  (let* ((b (cl-cc/binary:make-mach-o-builder :x86-64))
         (code (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#xC3 0 0 0))))
    (cl-cc/binary:add-text-segment b code)
    (cl-cc/binary:add-entry-point b 0)
    (let* ((result (cl-cc/binary:build-mach-o b code))
           (main-off (+ 32 72 232 152 72 32))
           (entryoff (+ (aref result (+ main-off 8))
                        (ash (aref result (+ main-off 9)) 8)
                        (ash (aref result (+ main-off 10)) 16)
                        (ash (aref result (+ main-off 11)) 24))))
      (expect entryoff :to-equal 24))))
