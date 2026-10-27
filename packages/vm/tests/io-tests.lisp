;;;; tests/unit/vm/io-tests.lisp — VM I/O Operations Unit Tests
;;;;
;;;; Tests for I/O instructions: make-string-output-stream,
;;;; get-output-stream-string, stream-write-string, read-from-string,
;;;; stream predicates, file-handle helpers, and read/write line behavior.

(in-package :cl-cc/test)



;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun io-vm (&optional (out (make-string-output-stream)))
  "Create a vm-io-state with a string output stream for capture."
  (cl-cc/vm:make-vm-state :output-stream out))

(defun io-vm-full (&optional (out (make-string-output-stream)))
  "Create a vm-io-state (with file handle management) for IO tests."
  (cl-cc/vm:make-vm-state :output-stream out))

(defun io-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc/vm:execute-instruction inst state 0 (make-hash-table :test #'equal)))

;;; ─── make-string-output-stream / get-output-stream-string ─────────────────

;;; ─── read-from-string ─────────────────────────────────────────────────────

(defun %io-read-str (src)
  "Execute vm-read-from-string on SRC in a fresh vm-state and return the result."
  (let ((vm (io-vm)))
    (cl-cc/vm:vm-reg-set vm :R1 src)
    (io-exec (cl-cc:make-vm-read-from-string-inst :dst :R0 :src :R1) vm)
    (cl-cc/vm:vm-reg-get vm :R0)))

;;; ─── vm-allocate-file-handle ────────────────────────────────────────────────

;;; ─── vm-get-stream ──────────────────────────────────────────────────────────

;;; ─── vm-stream-open-p ──────────────────────────────────────────────────────

;;; ─── stream predicate instructions ─────────────────────────────────────────

;;; ─── handle-based string streams (vm-make-string-stream) ───────────────────

;;; ─── eof-p ──────────────────────────────────────────────────────────────────

;;; ─── read-char / read-line via handle ──────────────────────────────────────

;;; ─── %resolve-integer-stream-handle (extracted helper) ──────────────────

;;; ─── %copy-ht-into ────────────────────────────────────────────────────────

;;; ─── clone-vm-state ───────────────────────────────────────────────────────
;;; ─── FR-868: file-position / file-length / with-binary-file ───────────────

(it-sequential "io-with-binary-file-defaults-to-io"
  (let ((path (merge-pathnames (format nil "clcc-with-binary-file-~A.bin" (gensym))
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (cl-cc/vm::with-binary-file (stream path :if-exists :supersede :if-does-not-exist :create)
             (write-byte 65 stream)
             (expect (file-position stream 0) :to-equal t)
             (expect (= 65 (read-byte stream)) :to-be-truthy))
           (expect (probe-file path) :to-be-truthy))
      (when (probe-file path) (delete-file path)))))

;;; ─── FR-923: buffered I/O and stream predicates ───────────────────────────

;;; ─── FR-924: special streams ───────────────────────────────────────────────

;;; ─── FR-927: pathname operations ───────────────────────────────────────────

;;; ─── FR-851/852/853/869: networking, DNS, TLS, mmap ───────────────────────

