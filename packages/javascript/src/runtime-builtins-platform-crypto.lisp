;;;; packages/javascript/src/runtime-builtins-platform-crypto.lisp — Crypto helpers

(in-package :cl-cc/javascript)

(defun %js-crypto-random-bytes (count)
  "Return COUNT bytes from the host CSPRNG, with a non-crypto fallback."
  (let ((bytes (make-array count :element-type '(unsigned-byte 8))))
    (handler-case
        (with-open-file (in "/dev/urandom" :direction :input
                                          :element-type '(unsigned-byte 8))
          (unless (= count (read-sequence bytes in))
            (error "short read from host CSPRNG")))
      (error ()
        (dotimes (i count)
          (setf (aref bytes i) (random #x100)))))
    bytes))

(defun %js-crypto-random-raw-integer (type-name)
  "Return random integer bits sized for TYPE-NAME."
  (let ((size (case (%js-ta-type-tag type-name)
                ((:int8 :uint8 :uint8c) 1)
                ((:int16 :uint16)       2)
                ((:int32 :uint32)       4)
                ((:bigint64 :biguint64) 8)
                ((:float32 :float64)
                 (error "JS TypeError: crypto.getRandomValues requires an integer TypedArray"))
                (t 1))))
    (loop with value = 0
          for byte across (%js-crypto-random-bytes size)
          do (setf value (logior (ash value 8) byte))
          finally (return value))))

(defun %js-crypto-random-uuid ()
  "Return a Web Crypto style RFC 4122 version 4 UUID string."
  (let ((bytes (%js-crypto-random-bytes 16)))
    (setf (aref bytes 6) (logior #x40 (logand (aref bytes 6) #x0f))
          (aref bytes 8) (logior #x80 (logand (aref bytes 8) #x3f)))
    (string-downcase
     (format nil "~2,'0x~2,'0x~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x"
             (aref bytes 0) (aref bytes 1) (aref bytes 2) (aref bytes 3)
             (aref bytes 4) (aref bytes 5)
             (aref bytes 6) (aref bytes 7)
             (aref bytes 8) (aref bytes 9)
             (aref bytes 10) (aref bytes 11) (aref bytes 12)
             (aref bytes 13) (aref bytes 14) (aref bytes 15)))))

(defun %js-crypto-integer-typed-array-p (arr)
  "Return true when ARR is an integer TypedArray accepted by getRandomValues."
  (and (js-typed-array-p arr)
       (not (member (%js-ta-type-tag (js-ta-type-name arr))
                    '(:float32 :float64)))))

(defun %js-crypto-get-random-values (arr)
  "Fill integer TypedArray ARR with random values and return ARR."
  (unless (%js-crypto-integer-typed-array-p arr)
    (error "JS TypeError: crypto.getRandomValues requires an integer TypedArray"))
  (let ((byte-length (* (js-ta-length arr) (js-ta-element-size arr))))
    (when (> byte-length 65536)
      (error "JS QuotaExceededError: crypto.getRandomValues byteLength exceeds 65536")))
  (let ((type-name (js-ta-type-name arr))
        (buffer (js-ta-buffer arr)))
    (dotimes (i (js-ta-length arr))
      (setf (aref buffer i)
            (%js-ta-coerce-element
             type-name
             (%js-crypto-random-raw-integer type-name)))))
  arr)

(defun %js-make-crypto ()
  "crypto global object (getRandomValues + randomUUID)."
  (%js-make-object
   "getRandomValues" #'%js-crypto-get-random-values
   "randomUUID" #'%js-crypto-random-uuid))
