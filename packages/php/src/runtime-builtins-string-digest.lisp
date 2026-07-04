;;;; PHP string digest builtins.

(in-package :cl-cc/php)

(defun %php-u32 (value)
  (logand value #xffffffff))

(defun %php-rol32 (value count)
  (let ((value (%php-u32 value)))
    (%php-u32 (logior (ash value count)
                      (ash value (- count 32))))))

(defun %php-digest-result (bytes raw)
  (if raw
      (with-output-to-string (out)
        (loop for byte across bytes
              do (write-char (code-char byte) out)))
      (%php-byte-vector-hex bytes)))

(defun %php-md5-padding (input)
  (let* ((length (length input))
         (bit-length (* length 8))
         (padded-length (+ length 1 8)))
    (loop while (/= (mod padded-length 64) 0)
          do (incf padded-length))
    (let ((bytes (make-array padded-length
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
      (replace bytes input)
      (setf (aref bytes length) #x80)
      (loop for i from 0 below 8
            do (setf (aref bytes (+ (- padded-length 8) i))
                     (logand (ash bit-length (* -8 i)) #xff)))
      bytes)))

(defun %php-read-u32-le (bytes index)
  (%php-u32 (logior (aref bytes index)
                    (ash (aref bytes (+ index 1)) 8)
                    (ash (aref bytes (+ index 2)) 16)
                    (ash (aref bytes (+ index 3)) 24))))

(defun %php-write-u32-le (bytes index value)
  (let ((value (%php-u32 value)))
    (loop for i from 0 below 4
          do (setf (aref bytes (+ index i))
                   (logand (ash value (* -8 i)) #xff)))))

(defun %php-md5-bytes (input)
  (let ((a0 #x67452301)
        (b0 #xefcdab89)
        (c0 #x98badcfe)
        (d0 #x10325476)
        (bytes (%php-md5-padding input)))
    (loop for offset from 0 below (length bytes) by 64
          do (let ((words (make-array 16)))
               (loop for i from 0 below 16
                     do (setf (aref words i)
                              (%php-read-u32-le bytes (+ offset (* i 4)))))
               (let ((a a0)
                     (b b0)
                     (c c0)
                     (d d0))
                 (loop for i from 0 below 64
                       do (multiple-value-bind (f g)
                              (cond ((< i 16)
                                     (values (%php-u32 (logior (logand b c)
                                                               (logand (lognot b) d)))
                                             i))
                                    ((< i 32)
                                     (values (%php-u32 (logior (logand d b)
                                                               (logand (lognot d) c)))
                                             (mod (+ (* 5 i) 1) 16)))
                                    ((< i 48)
                                     (values (%php-u32 (logxor b c d))
                                             (mod (+ (* 3 i) 5) 16)))
                                    (t
                                     (values (%php-u32 (logxor c (logior b (lognot d))))
                                             (mod (* 7 i) 16))))
                            (let ((new-b (%php-u32
                                          (+ b (%php-rol32
                                                (+ a f
                                                   (aref +php-md5-round-constants+ i)
                                                   (aref words g))
                                                (aref +php-md5-shift-amounts+ i))))))
                              (setf a d
                                    d c
                                    c b
                                    b new-b))))
                 (setf a0 (%php-u32 (+ a0 a))
                       b0 (%php-u32 (+ b0 b))
                       c0 (%php-u32 (+ c0 c))
                       d0 (%php-u32 (+ d0 d))))))
    (let ((digest (make-array 16 :element-type '(unsigned-byte 8))))
      (%php-write-u32-le digest 0 a0)
      (%php-write-u32-le digest 4 b0)
      (%php-write-u32-le digest 8 c0)
      (%php-write-u32-le digest 12 d0)
      digest)))

(defun %php-sha1-padding (input)
  (let* ((length (length input))
         (bit-length (* length 8))
         (padded-length (+ length 1 8)))
    (loop while (/= (mod padded-length 64) 0)
          do (incf padded-length))
    (let ((bytes (make-array padded-length
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
      (replace bytes input)
      (setf (aref bytes length) #x80)
      (loop for i from 0 below 8
            do (setf (aref bytes (- padded-length 1 i))
                     (logand (ash bit-length (* -8 i)) #xff)))
      bytes)))

(defun %php-read-u32-be (bytes index)
  (%php-u32 (logior (ash (aref bytes index) 24)
                    (ash (aref bytes (+ index 1)) 16)
                    (ash (aref bytes (+ index 2)) 8)
                    (aref bytes (+ index 3)))))

(defun %php-write-u32-be (bytes index value)
  (let ((value (%php-u32 value)))
    (loop for i from 0 below 4
          do (setf (aref bytes (+ index i))
                   (logand (ash value (* -8 (- 3 i))) #xff)))))

(defun %php-sha1-bytes (input)
  (let ((h0 #x67452301)
        (h1 #xefcdab89)
        (h2 #x98badcfe)
        (h3 #x10325476)
        (h4 #xc3d2e1f0)
        (bytes (%php-sha1-padding input)))
    (loop for offset from 0 below (length bytes) by 64
          do (let ((words (make-array 80 :initial-element 0)))
               (loop for i from 0 below 16
                     do (setf (aref words i)
                              (%php-read-u32-be bytes (+ offset (* i 4)))))
               (loop for i from 16 below 80
                     do (setf (aref words i)
                              (%php-rol32 (logxor (aref words (- i 3))
                                                  (aref words (- i 8))
                                                  (aref words (- i 14))
                                                  (aref words (- i 16)))
                                          1)))
               (let ((a h0)
                     (b h1)
                     (c h2)
                     (d h3)
                     (e h4))
                 (loop for i from 0 below 80
                       do (multiple-value-bind (f k)
                              (cond ((< i 20)
                                     (values (%php-u32 (logior (logand b c)
                                                               (logand (lognot b) d)))
                                             #x5a827999))
                                    ((< i 40)
                                     (values (%php-u32 (logxor b c d))
                                             #x6ed9eba1))
                                    ((< i 60)
                                     (values (%php-u32 (logior (logand b c)
                                                               (logand b d)
                                                               (logand c d)))
                                             #x8f1bbcdc))
                                    (t
                                     (values (%php-u32 (logxor b c d))
                                             #xca62c1d6)))
                            (let ((temp (%php-u32 (+ (%php-rol32 a 5)
                                                     f e k (aref words i)))))
                              (setf e d
                                    d c
                                    c (%php-rol32 b 30)
                                    b a
                                    a temp))))
                 (setf h0 (%php-u32 (+ h0 a))
                       h1 (%php-u32 (+ h1 b))
                       h2 (%php-u32 (+ h2 c))
                       h3 (%php-u32 (+ h3 d))
                       h4 (%php-u32 (+ h4 e))))))
    (let ((digest (make-array 20 :element-type '(unsigned-byte 8))))
      (%php-write-u32-be digest 0 h0)
      (%php-write-u32-be digest 4 h1)
      (%php-write-u32-be digest 8 h2)
      (%php-write-u32-be digest 12 h3)
      (%php-write-u32-be digest 16 h4)
      digest)))

(defun %php-md5 (str &optional raw)
  "PHP md5: return the MD5 hash of STR."
  (%php-digest-result (%php-md5-bytes (%php-string-bytes str)) raw))

(defun %php-sha1 (str &optional raw)
  "PHP sha1: return the SHA1 hash of STR."
  (%php-digest-result (%php-sha1-bytes (%php-string-bytes str)) raw))

(defun %php-crc32 (str)
  "PHP crc32: CRC-32 checksum."
  (let ((crc #xFFFFFFFF))
    (loop for byte across (%php-string-bytes str)
          do (setf crc (logxor crc byte))
             (loop repeat 8
                   do (setf crc
                            (%php-u32
                             (if (logbitp 0 crc)
                                 (logxor (ash crc -1) #xedb88320)
                                 (ash crc -1))))))
    (%php-u32 (logxor crc #xFFFFFFFF))))
