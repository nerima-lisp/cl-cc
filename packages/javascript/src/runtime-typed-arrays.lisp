;;;; packages/javascript/src/runtime-typed-arrays.lisp — JS TypedArray (ES2015+)
;;;;
;;;; TypedArrays are backed by CL arrays of the appropriate element type.
;;;; Supported: Int8Array, Uint8Array, Uint8ClampedArray,
;;;;            Int16Array, Uint16Array, Int32Array, Uint32Array,
;;;;            Float16Array, Float32Array, Float64Array,
;;;;            BigInt64Array, BigUint64Array.
;;;;
;;;; Each TypedArray is a struct wrapping a CL array + a type tag.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  TypedArray struct
;;; -----------------------------------------------------------------------

(defstruct (js-typed-array (:conc-name js-ta-))
  type-name        ; string: "Uint8Array" etc.
  element-size     ; bytes per element
  buffer           ; CL array (element-type determined by type-name)
  byte-offset      ; offset into buffer (usually 0)
  length)          ; number of elements

(defun %js-typed-array-p (x) (js-typed-array-p x))

;;; -----------------------------------------------------------------------
;;;  Constructor helpers
;;; -----------------------------------------------------------------------

(defparameter *js-typed-array-specs*
  '(("Int8Array"          :int8    1 (signed-byte 8)    -128                  127)
    ("Uint8Array"         :uint8   1 (unsigned-byte 8)   0                    255)
    ("Uint8ClampedArray"  :uint8c  1 (unsigned-byte 8)   0                    255)
    ("Int16Array"         :int16   2 (signed-byte 16)   -32768                32767)
    ("Uint16Array"        :uint16  2 (unsigned-byte 16)  0                    65535)
    ("Int32Array"         :int32   4 (signed-byte 32)   -2147483648           2147483647)
    ("Uint32Array"        :uint32  4 (unsigned-byte 32)  0                    4294967295)
    ("Float16Array"       :float16 2 double-float        nil                  nil)
    ("Float32Array"       :float32 4 single-float        nil                  nil)
    ("Float64Array"       :float64 8 double-float         nil                  nil)
    ;; ES2020 BigInt typed arrays (backed by signed-byte 64 / unsigned-byte 64)
    ("BigInt64Array"      :bigint64  8 (signed-byte 64)  -9223372036854775808  9223372036854775807)
    ("BigUint64Array"     :biguint64 8 (unsigned-byte 64) 0                   18446744073709551615))
  "TypedArray type specs: (name tag bytes cl-type min max).")

(defun %js-ta-clamp (val min max)
  "Clamp VAL to [MIN, MAX] for Uint8ClampedArray."
  (if (or (null min) (null max)) val
      (max min (min max (truncate val)))))

(defun %js-ta-type-tag (type)
  "Return the internal TypedArray type tag for TYPE, accepting names or tags."
  (if (keywordp type)
      type
      (let ((spec (find type *js-typed-array-specs* :test #'string= :key #'car)))
        (if spec (second spec) :uint8))))

(defun %js-ta-integer-number (val)
  "Coerce VAL through JS numeric conversion for integer TypedArray storage."
  (let ((n (%js-to-number val)))
    (if (or (%js-nan-p n) (%js-float-infinity-p n))
        0
        (truncate n))))

(defun %js-ta-coerce-element (type-tag val)
  "Coerce VAL to the appropriate element type for TYPE-TAG."
  (case (%js-ta-type-tag type-tag)
    (:float16 (%js-f16round-number val))
    (:float32 (coerce (%js-to-number val) 'single-float))
    (:float64 (coerce (%js-to-number val) 'double-float))
    (:bigint64
     (let ((v (if (js-bigint-p val) (js-bigint-value val) (%js-ta-integer-number val))))
       (let ((x (logand v #xFFFFFFFFFFFFFFFF)))
         (if (>= x 9223372036854775808) (- x 18446744073709551616) x))))
    (:biguint64
     (let ((v (if (js-bigint-p val) (js-bigint-value val) (%js-ta-integer-number val))))
       (logand v #xFFFFFFFFFFFFFFFF)))
    (t
     (let ((n (%js-ta-integer-number val)))
       (case (%js-ta-type-tag type-tag)
         (:int8    (let ((x (logand n #xFF)))
                     (if (>= x 128) (- x 256) x)))
         (:uint8   (logand n #xFF))
         (:uint8c  (%js-ta-clamp n 0 255))
         (:int16   (let ((x (logand n #xFFFF)))
                     (if (>= x 32768) (- x 65536) x)))
         (:uint16  (logand n #xFFFF))
         (:int32   (let ((x (logand n #xFFFFFFFF)))
                     (if (>= x 2147483648) (- x 4294967296) x)))
         (:uint32  (logand n #xFFFFFFFF))
         (t n))))))

(defun %js-make-typed-array (type-name &optional (arg +js-undefined+))
  "Construct a TypedArray of TYPE-NAME.
ARG can be: length (integer), another TypedArray, an array, or an iterable."
  (let* ((spec (find type-name *js-typed-array-specs* :test #'string= :key #'car))
         (tag  (if spec (second spec) :uint8))
         (esz  (if spec (third spec)  1))
         (n (cond
              ((or (eq arg +js-undefined+) (eq arg +js-null+)) 0)
              ((numberp arg) (truncate arg))
              ((js-typed-array-p arg) (js-ta-length arg))
              ((%js-vec-p arg) (length arg))
              (t 0)))
         (buf (make-array n :element-type t :initial-element 0)))
    ;; Copy elements if source is a TypedArray or plain array
    (cond
      ((js-typed-array-p arg)
       (let ((src-buf (js-ta-buffer arg))
             (src-tag (js-ta-type-name arg)))
         (dotimes (i (min n (js-ta-length arg)))
           (setf (aref buf i)
                 (%js-ta-coerce-element tag (aref src-buf i))))))
      ((%js-vec-p arg)
       (dotimes (i (min n (length arg)))
         (setf (aref buf i) (%js-ta-coerce-element tag (aref arg i))))))
    (make-js-typed-array :type-name type-name :element-size esz
                          :buffer buf :byte-offset 0 :length n)))

;;; -----------------------------------------------------------------------
;;;  Element access
;;; -----------------------------------------------------------------------

(defun %js-ta-get (ta index)
  "Get element at INDEX from TypedArray TA."
  (let ((i (truncate (%js-to-number index))))
    (if (and (>= i 0) (< i (js-ta-length ta)))
        (aref (js-ta-buffer ta) i)
        +js-undefined+)))

(defun %js-ta-set (ta index value)
  "Set element at INDEX in TypedArray TA to VALUE."
  (let ((i (truncate (%js-to-number index))))
    (when (and (>= i 0) (< i (js-ta-length ta)))
      (setf (aref (js-ta-buffer ta) i)
            (%js-ta-coerce-element (js-ta-type-name ta) value))))
  value)

;;; Prototype methods and ES2023/ES2025 methods live in
;;; runtime-typed-arrays-methods.lisp.
