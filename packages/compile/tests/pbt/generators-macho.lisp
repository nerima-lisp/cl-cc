;;;; tests/pbt/generators-macho.lisp - Mach-O Binary Structure Generators
;;;
;;; Generators for Mach-O binary format structures, built on cl-weave 1.0.0's
;;; combinators. Type expression generators are in generators-types.lisp.
;;;
;;; These live in CL-CC/PBT rather than a dedicated CL-CC/PBT-MACHO package.
;;; That package existed only to re-export ~50 constants, struct accessors and
;;; generators to the single file that consumed them, all inside the same test
;;; system; with the generators on cl-weave there is nothing left for it to
;;; mediate, so package-macho.lisp is gone.
;;;
;;; Only load-command types that a property actually inspects are kept.
;;; GEN-MACH-LOAD-COMMAND-TYPE had no callers and is dropped.
;;;
;;; Note: these generators describe Mach-O structures for *generator* testing.
;;; cl-cc's real Mach-O emitter, and its own copies of these constants, live in
;;; packages/binary/src/macho.lisp and are covered by
;;; packages/emit/tests/macho-tests.lisp.

(in-package :cl-cc/pbt)

(defparameter *max-mach-o-sections* 5
  "Maximum number of sections in generated Mach-O segments.")

;;; Mach-O constants

;; Magic numbers
(defconstant +mh-magic+ #xFEEDFACE "32-bit Mach-O")
(defconstant +mh-magic-64+ #xFEEDFACF "64-bit Mach-O")
(defconstant +mh-cigam+ #xCEFAEDFE "32-bit Mach-O byte-swapped")
(defconstant +mh-cigam-64+ #xCFFAEDFE "64-bit Mach-O byte-swapped")

;; CPU types
(defconstant +cpu-type-x86+ 7)
(defconstant +cpu-type-x86-64+ #x01000007)
(defconstant +cpu-type-arm+ 12)
(defconstant +cpu-type-arm64+ #x0100000C)

;; CPU subtypes
(defconstant +cpu-subtype-x86-all+ 3)
(defconstant +cpu-subtype-arm-all+ 0)

;; File types
(defconstant +mh-object+ 1 "Relocatable object file")
(defconstant +mh-execute+ 2 "Executable file")
(defconstant +mh-core+ 4 "Core file")
(defconstant +mh-preload+ 5 "Preloaded executable")
(defconstant +mh-dylib+ 6 "Dynamic library")
(defconstant +mh-bundle+ 8 "Dynamic bundle")

;; Header flags
(defconstant +mh-noundefs+ 1 "No undefined references")
(defconstant +mh-dyldlink+ 4 "Dyld will link this")
(defconstant +mh-pie+ #x200000 "Position-independent executable")

;; Load command types
(defconstant +lc-segment-64+ #x19)

;; Segment protection flags
(defconstant +vm-prot-read+ 1)
(defconstant +vm-prot-write+ 2)
(defconstant +vm-prot-execute+ 4)

;;; Mach-O structures

(defstruct (mach-header (:constructor make-mach-header-raw))
  "Structure representing a Mach-O header."
  magic cputype cpusubtype filetype ncmds sizeofcmds flags reserved)

(defstruct (mach-segment-command (:constructor make-mach-segment-raw))
  "Structure representing a Mach-O segment command."
  cmd cmdsize segname vmaddr vmsize fileoff filesize maxprot initprot nsects flags sections)

(defstruct (mach-section (:constructor make-mach-section-raw))
  "Structure representing a Mach-O section."
  sectname segname addr size offset align reloff nreloc flags reserved1 reserved2 reserved3)

;;; Field generators

(defun gen-mach-magic ()
  "Generate valid Mach-O magic numbers."
  (cl-weave:gen-member (list +mh-magic+ +mh-magic-64+ +mh-cigam+ +mh-cigam-64+)))

(defun gen-mach-cpu-type ()
  "Generate valid CPU types for Mach-O."
  (cl-weave:gen-member (list +cpu-type-x86+ +cpu-type-x86-64+
                             +cpu-type-arm+ +cpu-type-arm64+)))

(defun gen-mach-cpu-subtype ()
  "Generate valid CPU subtypes for Mach-O."
  (cl-weave:gen-member (list +cpu-subtype-x86-all+ +cpu-subtype-arm-all+)))

(defun gen-mach-file-type ()
  "Generate valid Mach-O file types."
  (cl-weave:gen-member (list +mh-object+ +mh-execute+ +mh-dylib+
                             +mh-bundle+ +mh-preload+ +mh-core+)))

(defun gen-mach-flags ()
  "Generate valid Mach-O header flags as an OR-ed bitmask."
  (cl-weave:gen-map
   (lambda (flag-list) (reduce #'logior flag-list :initial-value 0))
   (cl-weave:gen-list (cl-weave:gen-member (list +mh-noundefs+ +mh-dyldlink+ +mh-pie+ 0))
                      :min-length 0 :max-length 3)))

(defun gen-segment-permissions ()
  "Generate valid segment permissions (rwx) as an OR-ed bitmask in [0,7]."
  (cl-weave:gen-map
   (lambda (perms) (reduce #'logior perms :initial-value 0))
   (cl-weave:gen-list (cl-weave:gen-member (list +vm-prot-read+ +vm-prot-write+
                                                 +vm-prot-execute+))
                      :min-length 1 :max-length 3)))

(defun gen-segment-name ()
  "Generate valid Mach-O segment names."
  (cl-weave:gen-member '("__TEXT" "__DATA" "__LINKEDIT" "__OBJC" "__IMPORT"
                         "__LC_SEGMENT")))

(defun gen-section-name ()
  "Generate valid Mach-O section names."
  (cl-weave:gen-member '("__text" "__data" "__bss" "__const" "__cstring"
                         "__literal4" "__literal8" "__mod_init_func"
                         "__mod_term_func" "__objc_classlist")))

;;; Structure generators

(defun gen-mach-header ()
  "Generate a valid Mach-O header.
RESERVED is present only for 64-bit magics, which is a pure function of the
generated magic — the original threaded it through nested GEN-BINDs, but nothing
about the remaining fields depends on it, so a single GEN-MAP suffices."
  (cl-weave:gen-map
   (lambda (parts)
     (destructuring-bind (magic cpu subtype filetype flags ncmds sizeofcmds) parts
       (make-mach-header-raw
        :magic magic :cputype cpu :cpusubtype subtype :filetype filetype
        :ncmds ncmds :sizeofcmds sizeofcmds :flags flags
        :reserved (when (or (= magic +mh-magic-64+) (= magic +mh-cigam-64+)) 0))))
   (cl-weave:gen-tuple (gen-mach-magic) (gen-mach-cpu-type) (gen-mach-cpu-subtype)
                       (gen-mach-file-type) (gen-mach-flags)
                       (cl-weave:gen-integer :min 1 :max 10)
                       (cl-weave:gen-integer :min 32 :max 4096))))

(defun gen-mach-section ()
  "Generate a valid Mach-O section."
  (cl-weave:gen-map
   (lambda (parts)
     (destructuring-bind (sectname segname addr size offset align flags) parts
       (make-mach-section-raw
        :sectname sectname :segname segname :addr addr :size size :offset offset
        :align align :reloff 0 :nreloc 0 :flags flags
        :reserved1 0 :reserved2 0 :reserved3 nil)))
   (cl-weave:gen-tuple (gen-section-name) (gen-segment-name)
                       (cl-weave:gen-integer :min 0 :max #xFFFFFF)
                       (cl-weave:gen-integer :min 0 :max #xFFFF)
                       (cl-weave:gen-integer :min 512 :max #xFFFFF)
                       (cl-weave:gen-member '(0 1 2 3 4))
                       (cl-weave:gen-integer :min 0 :max #xFFFFFFFF))))

(defun gen-mach-segment-command ()
  "Generate a valid Mach-O 64-bit segment command.
NSECTS and CMDSIZE are derived from the generated section list, so the
nsects-matches-sections invariant holds by construction."
  (cl-weave:gen-map
   (lambda (parts)
     (destructuring-bind (maxprot initprot segname vmaddr vmsize fileoff filesize sections)
         parts
       (make-mach-segment-raw
        :cmd +lc-segment-64+ :cmdsize (+ 72 (* 80 (length sections)))
        :segname segname :vmaddr vmaddr :vmsize vmsize :fileoff fileoff
        :filesize filesize :maxprot maxprot :initprot initprot
        :nsects (length sections) :flags 0 :sections sections)))
   (cl-weave:gen-tuple (gen-segment-permissions) (gen-segment-permissions)
                       (gen-segment-name)
                       (cl-weave:gen-integer :min 0 :max #xFFFFFFFFFF)
                       (cl-weave:gen-integer :min 0 :max #xFFFFFFFFFF)
                       (cl-weave:gen-integer :min 0 :max #xFFFFFFFFFF)
                       (cl-weave:gen-integer :min 0 :max #xFFFFF)
                       (cl-weave:gen-list (gen-mach-section)
                                          :min-length 0
                                          :max-length *max-mach-o-sections*))))
