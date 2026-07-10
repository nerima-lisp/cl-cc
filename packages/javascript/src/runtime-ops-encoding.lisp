;;;; packages/javascript/src/runtime-ops-encoding.lisp — URI / base64 / text codecs
;;;;
;;;; Separated from runtime-ops.lisp so the core operator helpers remain focused
;;;; on arithmetic, bitwise, bigint, and control-flow-adjacent primitives.

(in-package :cl-cc/javascript)

;;; ─── URI encoding/decoding ───────────────────────────────────────────────────
;;;
;;; Both encode functions share the same percent-encoding loop — they differ
;;; only in the set of "safe" characters that are passed through unchanged.
;;; +uri-component-safe-chars+ omits the URI-structure chars that encodeURI
;;; must preserve.

(defparameter +uri-component-safe-chars+
  '(#\- #\_ #\. #\! #\~ #\* #\' #\( #\))
  "Characters that encodeURIComponent leaves unencoded (RFC 3986 unreserved).")

(defparameter +uri-safe-chars+
  (append +uri-component-safe-chars+
          '(#\; #\/ #\? #\: #\@ #\& #\= #\+ #\$ #\, #\#))
  "Characters that encodeURI leaves unencoded (unreserved + URI structure chars).")

(defun %js-percent-encode (str safe-chars)
  "Percent-encode STR, leaving alphanumerics and SAFE-CHARS unchanged."
  (with-output-to-string (out)
    (loop for ch across (%js-to-string str)
          do (if (or (alphanumericp ch) (member ch safe-chars))
                 (write-char ch out)
                 (loop for byte across (sb-ext:string-to-octets (string ch) :external-format :utf-8)
                       do (format out "%~2,'0X" byte))))))

(defun %js-encode-uri-component (str)
  "JS encodeURIComponent: percent-encode all chars except unreserved."
  (%js-percent-encode str +uri-component-safe-chars+))

(defun %js-decode-uri-component (str)
  "JS decodeURIComponent: decode percent-encoded string."
  (let ((s     (%js-to-string str))
        (bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop with i = 0 and n = (length s)
          while (< i n)
          do (let ((ch (char s i)))
               (if (and (char= ch #\%) (< (+ i 2) n))
                   (progn (vector-push-extend
                           (parse-integer s :start (1+ i) :end (+ i 3) :radix 16) bytes)
                          (incf i 3))
                   (progn (loop for b across (sb-ext:string-to-octets (string ch) :external-format :utf-8)
                                do (vector-push-extend b bytes))
                          (incf i)))))
    (sb-ext:octets-to-string bytes :external-format :utf-8)))

(defun %js-encode-uri (str)
  "JS encodeURI: encode URI, preserving scheme/path/query chars."
  (%js-percent-encode str +uri-safe-chars+))

(defun %js-decode-uri (str)
  "JS decodeURI: decode URI but leave reserved chars encoded."
  (%js-decode-uri-component str))

;;; ─── atob / btoa (base64 in browsers) ───────────────────────────────────────

(defun %js-btoa (str)
  "JS btoa: base64-encode a binary string."
  (let* ((s (%js-to-string str))
         (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (with-output-to-string (out)
      (let ((bytes (map 'vector #'char-code s))
            (n (length s)))
        (loop for i from 0 below n by 3
              do (let* ((b0 (aref bytes i))
                        (b1 (if (< (1+ i) n) (aref bytes (1+ i)) 0))
                        (b2 (if (< (+ i 2) n) (aref bytes (+ i 2)) 0)))
                   (write-char (char alphabet (ash b0 -2)) out)
                   (write-char (char alphabet (logior (ash (logand b0 3) 4) (ash b1 -4))) out)
                   (write-char (if (< (1+ i) n) (char alphabet (logior (ash (logand b1 15) 2) (ash b2 -6))) #\=) out)
                   (write-char (if (< (+ i 2) n) (char alphabet (logand b2 63)) #\=) out)))))))

(defun %js-atob (str)
  "JS atob: decode base64 string."
  (let* ((s (string-trim '(#\Space #\Tab #\Newline #\Return) (%js-to-string str)))
         (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (flet ((dc (ch) (or (position ch alphabet) 0)))
      (with-output-to-string (out)
        (loop for i from 0 below (length s) by 4
              while (< (+ i 3) (length s))
              do (let* ((g0 (dc (char s i)))       (g1 (dc (char s (1+ i))))
                        (g2 (if (char= (char s (+ i 2)) #\=) 0 (dc (char s (+ i 2)))))
                        (g3 (if (char= (char s (+ i 3)) #\=) 0 (dc (char s (+ i 3))))))
                   (write-char (code-char (logior (ash g0 2) (ash g1 -4))) out)
                   (unless (char= (char s (+ i 2)) #\=)
                     (write-char (code-char (logior (ash (logand g1 15) 4) (ash g2 -2))) out))
                   (unless (char= (char s (+ i 3)) #\=)
                     (write-char (code-char (logior (ash (logand g2 3) 6) g3)) out))))))))

;;; ─── TextEncoder / TextDecoder ───────────────────────────────────────────────

(defun %js-utf8-octets (value)
  (sb-ext:string-to-octets (%js-to-string value) :external-format :utf-8))

(defun %js-text-decoder-encoding (encoding)
  (let ((name (string-downcase
               (%js-to-string
                (if (or (null encoding)
                        (eq encoding +js-undefined+)
                        (eq encoding +js-null+))
                    "utf-8"
                    encoding)))))
    (cond
      ((or (string= name "utf8") (string= name "unicode-1-1-utf-8"))
       "utf-8")
      (t name))))

(defun %js-text-decode-octets (bytes)
  (handler-case
      (sb-ext:octets-to-string bytes :external-format :utf-8)
    (error () "")))

(defun %js-text-buffer-octets (buf)
  (cond
    ((js-typed-array-p buf)
     (let* ((len (js-ta-length buf))
            (bytes (make-array len :element-type '(unsigned-byte 8))))
       (dotimes (i len bytes)
         (setf (aref bytes i) (logand #xFF (truncate (%js-ta-get buf i)))))))
    ((vectorp buf)
     (let* ((len (length buf))
            (bytes (make-array len :element-type '(unsigned-byte 8))))
       (dotimes (i len bytes)
         (setf (aref bytes i) (logand #xFF (truncate (aref buf i)))))))
    (t nil)))

(defun %js-text-encode-into (str dest)
  (let* ((s (%js-to-string str))
         (dest-len (if (js-typed-array-p dest) (js-ta-length dest) 0))
         (read 0)
         (written 0))
    (when (js-typed-array-p dest)
      (loop for ch across s
            for ch-str = (string ch)
            for bytes = (sb-ext:string-to-octets ch-str :external-format :utf-8)
            for byte-count = (length bytes)
            while (<= (+ written byte-count) dest-len)
            do (progn
                 (dotimes (i byte-count)
                   (%js-ta-set dest (+ written i) (aref bytes i)))
                 (incf written byte-count)
                 (incf read (if (> (char-code ch) #xFFFF) 2 1)))))
    (%js-make-object "read" (coerce read 'double-float)
                     "written" (coerce written 'double-float))))

(defun %js-make-text-encoder ()
  "JS TextEncoder (UTF-8 encoding)."
  (%js-make-object
   "encoding" "utf-8"
   "encode"   (lambda (str)
                (let* ((bytes (%js-utf8-octets str))
                       (vec (%js-make-typed-array "Uint8Array" (length bytes))))
                  (loop for i below (length bytes)
                        do (%js-ta-set vec i (aref bytes i)))
                  vec))
   "encodeInto" #'%js-text-encode-into))

(defun %js-make-text-decoder (&optional (encoding "utf-8"))
  "JS TextDecoder for UTF-8 byte inputs."
  (let ((normalized (%js-text-decoder-encoding encoding)))
    (%js-make-object
     "encoding" normalized
     "fatal" nil
     "ignoreBOM" nil
     "decode"   (lambda (&optional buf _options)
                  (declare (ignore _options))
                  (cond
                    ((or (null buf) (eq buf +js-undefined+)) "")
                    (t
                     (let ((bytes (%js-text-buffer-octets buf)))
                       (if bytes
                           (%js-text-decode-octets bytes)
                           (%js-to-string buf)))))))))
