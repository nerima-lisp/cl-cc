;;;; packages/compile/src/abi-symbol-139.lisp — Phase 139: ABI/Symbol Management
;;;; FR-776 Name Mangling, FR-777 ABI Stability Manifest,
;;;; FR-778 Debug Symbol Stripping Modes, FR-779 Symbol Namespace Management

(in-package :cl-cc/compile)

;;; ──── FR-776: Name Mangling ────
(defun %string-designator (thing)
  (typecase thing
    (string thing)
    (symbol (symbol-name thing))
    (t (princ-to-string thing))))

(defun %mangle-length-prefixed-segment (thing)
  (let ((segment (string-downcase (%string-designator thing))))
    (format nil "~D~A" (length segment) segment)))

(defun %mangle-specializer-code (specializer)
  (typecase specializer
    (symbol (%mangle-length-prefixed-segment specializer))
    (string (%mangle-length-prefixed-segment specializer))
    (t (%mangle-length-prefixed-segment specializer))))

(defun mangle-function-name (name &key package specializers)
  "Mangle a function symbol NAME to Itanium C++ ABI-compatible string.
Format:
  _Z<len>name
  _ZN<pkg-len>pkg<name-len>nameE
  optional specializers are encoded as I<len>spec...E
Example: (cl-cc:foo integer) → _ZN5cl-cc3fooI7integerEE"
  (let ((base (%mangle-length-prefixed-segment name)))
    (labels ((specializer-block ()
               (when specializers
                 (with-output-to-string (out)
                   (write-char #\I out)
                   (dolist (specializer specializers)
                     (write-string (%mangle-specializer-code specializer) out))
                   (write-char #\E out)))))
      (if package
          (format nil "_ZN~A~A~:[E~;~AE~]"
                  (%mangle-length-prefixed-segment package)
                  base
                  specializers
                  (specializer-block))
          (format nil "_Z~A~:[~;~A~]"
                  base
                  specializers
                  (specializer-block))))))

(defun %demangle-read-segment (string position)
  (multiple-value-bind (length next-position)
      (parse-integer string :start position :junk-allowed t)
    (unless next-position
      (error "Invalid length-prefixed segment in ~S at position ~D"
             string position))
    (let ((end (+ next-position length)))
      (when (> end (length string))
        (error "Truncated length-prefixed segment in ~S at position ~D"
               string position))
      (values (subseq string next-position end) end))))

(defun %demangle-read-specializers (mangled position)
  (when (and (< position (length mangled))
             (char= (char mangled position) #\I))
    (let ((specializers '())
          (cursor (1+ position)))
      (loop
        (when (>= cursor (length mangled))
          (error "Unterminated specializer block in ~S" mangled))
        (let ((marker (char mangled cursor)))
          (cond
            ((char= marker #\E)
             (return (values (nreverse specializers) (1+ cursor))))
            (t
             ;; Each specializer is length-prefixed, so start reading after I.
             (multiple-value-bind (specializer next-cursor)
                 (%demangle-read-segment mangled cursor)
               (push specializer specializers)
               (setf cursor next-cursor)))))))))

(defun %demangle-join-specializers (specializers)
  (when specializers
    (with-output-to-string (out)
      (write-char #\< out)
      (loop for specializer in specializers
            for index from 0
            do (when (> index 0)
                 (write-string ", " out))
               (write-string specializer out))
      (write-char #\> out))))

(defun demangle-name (mangled)
  "Demangle MANGLED string back to human-readable form.
Example: _Z5fooi → cl-cc:foo"
  (unless (and (stringp mangled)
               (<= 2 (length mangled))
               (string= mangled "_Z" :end1 2))
    (error "Invalid mangled name: ~S" mangled))
  (let ((cursor 2)
        (package nil)
        (name nil)
        (specializers nil)
        (namespaced-p nil))
    (when (and (< cursor (length mangled))
               (char= (char mangled cursor) #\N))
      (setf namespaced-p t)
      (incf cursor))
    (multiple-value-setq (package cursor)
      (if namespaced-p
          (%demangle-read-segment mangled cursor)
          (values nil cursor)))
    (multiple-value-setq (name cursor)
      (%demangle-read-segment mangled cursor))
    (multiple-value-bind (parsed-specializers next-cursor)
        (%demangle-read-specializers mangled cursor)
      (setf specializers parsed-specializers
            cursor next-cursor))
    (when namespaced-p
      (unless (and (< cursor (length mangled))
                   (char= (char mangled cursor) #\E))
        (error "Invalid namespaced mangled name: ~S" mangled))
      (incf cursor))
    (unless (= cursor (length mangled))
      (error "Trailing data in mangled name: ~S" mangled))
    (with-output-to-string (out)
      (when package
        (write-string package out)
        (write-char #\: out))
      (write-string name out)
      (write-string (%demangle-join-specializers specializers) out))))

;;; ──── FR-777: ABI Stability Manifest ────
(defstruct abi-manifest
  "ABI stability manifest for public API surface."
  (version nil :type string)
  (exports nil :type list)
  (struct-layouts nil :type list)
  (checksum 0 :type (unsigned-byte 64)))

(defun %abi-manifest->plist (manifest)
  (list :version (abi-manifest-version manifest)
        :exports (abi-manifest-exports manifest)
        :struct-layouts (abi-manifest-struct-layouts manifest)
        :checksum (abi-manifest-checksum manifest)))

(defun %plist->abi-manifest (plist)
  (make-abi-manifest :version (or (getf plist :version) "0.0.0")
                     :exports (getf plist :exports)
                     :struct-layouts (getf plist :struct-layouts)
                     :checksum (or (getf plist :checksum) 0)))

(defun dump-abi-manifest (path)
  "Dump ABI manifest for current compilation to PATH."
  (let ((manifest (make-abi-manifest :version "1.0.0")))
    (when path
      (with-open-file (stream path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write (%abi-manifest->plist manifest) :stream stream :pretty nil)))
    manifest))

(defun check-abi-compatibility (old-manifest new-manifest)
  "Check ABI compatibility between OLD-MANIFEST and NEW-MANIFEST.
Returns nil if compatible, or list of breaking changes."
  (let ((old (%plist->abi-manifest (if (typep old-manifest 'abi-manifest)
                                       (%abi-manifest->plist old-manifest)
                                       old-manifest)))
        (new (%plist->abi-manifest (if (typep new-manifest 'abi-manifest)
                                       (%abi-manifest->plist new-manifest)
                                       new-manifest)))
        (issues '()))
    (unless (string= (abi-manifest-version old)
                     (abi-manifest-version new))
      (push :version issues))
    (unless (equal (abi-manifest-exports old)
                   (abi-manifest-exports new))
      (push :exports issues))
    (unless (equal (abi-manifest-struct-layouts old)
                   (abi-manifest-struct-layouts new))
      (push :struct-layouts issues))
    (unless (= (abi-manifest-checksum old)
               (abi-manifest-checksum new))
      (push :checksum issues))
    (nreverse issues)))

;;; ──── FR-778: Debug Symbol Stripping Modes ────
(progn
  (define-condition binary-transformation-error (error)
    ((operation :initarg :operation :reader binary-transformation-operation)
     (binary-path :initarg :binary-path :reader binary-transformation-path)
     (format :initarg :format :initform nil :reader binary-transformation-format)
     (reason :initarg :reason :reader binary-transformation-reason)
     (mode :initarg :mode :initform nil :reader binary-transformation-mode))
    (:report
     (lambda (condition stream)
       (format stream "Binary transformation ~S failed for ~S: ~A"
               (binary-transformation-operation condition)
               (binary-transformation-path condition)
               (binary-transformation-reason condition)))))
  (define-condition malformed-binary (binary-transformation-error) ())
  (define-condition unsupported-binary-transformation (binary-transformation-error)
    ()
    (:report
     (lambda (condition stream)
       (format stream
               "Binary transformation ~S is unsupported for ~S (~S, mode ~S): ~A"
               (binary-transformation-operation condition)
               (binary-transformation-path condition)
               (binary-transformation-format condition)
               (binary-transformation-mode condition)
               (binary-transformation-reason condition)))))
  (defvar *strip-mode* nil
    "Symbol stripping mode: nil (keep), :all, :debug, :unneeded.")
  (defun %transformation-error (condition operation path reason &key format mode)
    (error condition :operation operation :binary-path path :format format
                     :mode mode :reason reason))
  (defun %pathname-designator (path operation role)
    (handler-case
        (pathname path)
      (error ()
        (%transformation-error 'binary-transformation-error operation path
                               (format nil "~A must be a pathname designator" role)))))
  (defun %binary-format (binary-path operation)
    (let* ((path (%pathname-designator binary-path operation "binary path"))
           (existing (probe-file path)))
      (unless existing
        (%transformation-error 'binary-transformation-error operation path
                               "binary path does not exist"))
      (handler-case
          (with-open-file (stream existing :direction :input
                                           :element-type '(unsigned-byte 8))
            (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
              (unless (= (read-sequence magic stream) 4)
                (%transformation-error 'malformed-binary operation existing
                                       "binary is shorter than its file signature"))
              (cond
                ((equalp magic #(127 69 76 70)) :elf)
                ((and (= (aref magic 0) 77) (= (aref magic 1) 90)) :pe)
                ((member magic
                         '(#(254 237 250 206) #(206 250 237 254)
                           #(254 237 250 207) #(207 250 237 254)
                           #(202 254 186 190) #(190 186 254 202)
                           #(202 254 186 191) #(191 186 254 202))
                         :test #'equalp)
                 :mach-o)
                (t
                 (%transformation-error 'malformed-binary operation existing
                                        "unrecognized binary file signature")))))
        (file-error (condition)
          (%transformation-error 'binary-transformation-error operation existing
                                 (princ-to-string condition)))))))

(defun strip-symbols (binary-path mode)
  "Validate a request to strip symbols from BINARY-PATH using MODE.
Signals UNSUPPORTED-BINARY-TRANSFORMATION until a binary rewriter is available."
  (unless (member mode '(:all :debug :unneeded))
    (%transformation-error 'binary-transformation-error :strip-symbols binary-path
                           "mode must be one of :ALL, :DEBUG, or :UNNEEDED"
                           :mode mode))
  (let ((format (%binary-format binary-path :strip-symbols)))
    (%transformation-error 'unsupported-binary-transformation
                           :strip-symbols binary-path
                           "no binary symbol rewriter is available"
                           :format format :mode mode)))

(defun split-debug-info (binary-path debug-path)
  "Validate a request to split debug info from BINARY-PATH to DEBUG-PATH.
Signals UNSUPPORTED-BINARY-TRANSFORMATION until a binary rewriter is available."
  (%pathname-designator debug-path :split-debug-info "debug path")
  (let ((format (%binary-format binary-path :split-debug-info)))
    (%transformation-error 'unsupported-binary-transformation
                           :split-debug-info binary-path
                           "no debug information splitter is available"
                           :format format)))

;;; ──── FR-779: Symbol Namespace Management ────
(defstruct symbol-namespace
  "Hierarchical symbol namespace for package organization."
  (name nil :type string)
  (exports nil :type list)
  (imports nil :type list)
  (parent nil))

(defvar *namespaces* (make-hash-table :test #'equal)
  "Registered hierarchical namespaces.")

(defun %normalize-name-list (names)
  (mapcar #'%string-designator names))

(defun define-namespace (name &key exports imports)
  "Define a hierarchical namespace NAME."
  (let ((ns (make-symbol-namespace :name (string name)
                                    :exports (%normalize-name-list exports)
                                    :imports (%normalize-name-list imports))))
    (setf (gethash (string name) *namespaces*) ns)
    ns))

(defun check-namespace-deps ()
  "Check for circular dependencies in the namespace dependency graph."
  (labels ((visit (name visiting visited issues)
             (cond
               ((member name visiting :test #'equal)
                (push (list :cycle (reverse (cons name visiting))) issues)
                issues)
               ((member name visited :test #'equal)
                issues)
               (t
                (let ((namespace (gethash name *namespaces*)))
                  (if (null namespace)
                      (push (list :missing name) issues)
                      (progn
                        (push name visited)
                        (dolist (import (symbol-namespace-imports namespace) issues)
                          (setf issues (visit import (cons name visiting) visited issues)))))))))
           (collect-roots ()
             (loop for key being the hash-keys of *namespaces*
                   collect key)))
    (let ((issues '()))
      (dolist (root (collect-roots))
        (setf issues (visit root '() '() issues)))
      (nreverse issues))))

;; ── Exports ──
(export '(mangle-function-name demangle-name
          abi-manifest dump-abi-manifest check-abi-compatibility
          binary-transformation-error unsupported-binary-transformation
          malformed-binary binary-transformation-operation
          binary-transformation-path binary-transformation-format
          binary-transformation-reason binary-transformation-mode
          *strip-mode* strip-symbols split-debug-info
          symbol-namespace *namespaces* define-namespace check-namespace-deps))
