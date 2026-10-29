;;;; packages/binary/tests/binary-buffer-tests.lisp — Binary buffer operation tests
;;;;
;;;; Tests for: binary buffer write/read, alignment, string conversion,
;;;; serialization primitives (macho-buffer.lisp).

(in-package :cl-cc/test)



;;; ------------------------------------------------------------
;;; Binary Buffer — make-binary-buffer, write/read operations
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; byte-buffer — make-byte-buffer, buffer-write-byte
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; Utilities — align-up, string-to-ascii-bytes
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; Serialization — serialize-uint32-le, serialize-uint64-le
;;; ------------------------------------------------------------

(it-sequential "wasm-binary-write-f64 pos-1.0"
  (destructuring-bind (value expected) (list 1.0d0 '(0 0 0 0 0 0 #xf0 #x3f))
    (let ((buf (cl-cc/binary::make-binary-buffer)))
    (cl-cc/binary::binary-buffer-write-u64le buf (cl-cc/binary::portable-double-float-bits value))
    (expect (coerce (cl-cc/binary::binary-buffer-to-array buf) 'list) :to-equal expected))))

(it-sequential "wasm-binary-write-f64 neg-2.5"
  (destructuring-bind (value expected) (list -2.5d0 '(0 0 0 0 0 0 #x04 #xc0))
    (let ((buf (cl-cc/binary::make-binary-buffer)))
    (cl-cc/binary::binary-buffer-write-u64le buf (cl-cc/binary::portable-double-float-bits value))
    (expect (coerce (cl-cc/binary::binary-buffer-to-array buf) 'list) :to-equal expected))))

(it-sequential "wasm-binary-write-f64 neg-zero"
  (destructuring-bind (value expected) (list -0.0d0 '(0 0 0 0 0 0 0 #x80))
    (let ((buf (cl-cc/binary::make-binary-buffer)))
    (cl-cc/binary::binary-buffer-write-u64le buf (cl-cc/binary::portable-double-float-bits value))
    (expect (coerce (cl-cc/binary::binary-buffer-to-array buf) 'list) :to-equal expected))))
