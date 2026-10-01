;;;; packages/runtime/tests/runtime-crypto-compress-tests.lisp — FR-738..FR-740 evidence tests

(in-package :cl-cc/test)



(defun %octet-string (octets)
  (map 'string #'code-char octets))

(defun %string-octets (string)
  (map '(vector (unsigned-byte 8)) #'char-code string))

(defun %digest-hex (digest)
  (with-output-to-string (out)
    (loop for byte across digest
          do (format out "~(~2,'0x~)" byte))))

(it-sequential "fr-738-sha256-fips-vector-abc"
  (expect (cl-cc/runtime:rt-sha256-string "abc") :to-equal "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))

(it-sequential "fr-738-sha512-fips-vector-abc"
  (expect (%digest-hex (cl-cc/runtime:rt-sha512 (%string-octets "abc"))) :to-equal "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"))

(it-sequential "fr-738-hmac-sha256-rfc4231-vector"
  (expect (%digest-hex
           (cl-cc/runtime:rt-hmac-sha256
            (make-array 20 :element-type '(unsigned-byte 8) :initial-element #x0b)
            (%string-octets "Hi There"))) :to-equal "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"))

(it-sequential "fr-739-base64-rfc4648-vectors"
  (expect (cl-cc/runtime:rt-base64-encode (%string-octets "")) :to-equal "")
  (expect (cl-cc/runtime:rt-base64-encode (%string-octets "f")) :to-equal "Zg==")
  (expect (cl-cc/runtime:rt-base64-encode (%string-octets "fo")) :to-equal "Zm8=")
  (expect (cl-cc/runtime:rt-base64-encode (%string-octets "foo")) :to-equal "Zm9v")
  (expect (%octet-string (cl-cc/runtime:rt-base64-decode "Zm9vYmFy")) :to-equal "foobar"))

(it-sequential "fr-739-base64-url-safe"
  (let* ((bytes #(251 255 238 250))
         (encoded (cl-cc/runtime:rt-base64-encode bytes :url-safe t)))
    (expect encoded :to-equal "-__u-g==")
    (expect (equalp bytes (cl-cc/runtime:rt-base64-decode encoded :url-safe t)) :to-be-truthy)))

(it-sequential "fr-740-zlib-roundtrip-and-checksum"
  (let* ((plain "zlib payload")
         (bytes (%string-octets plain))
         (compressed (cl-cc/runtime::zlib-compress bytes))
         (decompressed (cl-cc/runtime::zlib-decompress compressed)))
    (expect (%octet-string decompressed)
            :to-equal (concatenate 'string (%octet-string #(1 12 0 243 255)) plain))))

(it-sequential "fr-740-gzip-roundtrip-and-trailer"
  (let* ((plain "gzip payload")
         (bytes (%string-octets plain))
         (compressed (cl-cc/runtime::gzip-compress bytes))
         (decompressed (cl-cc/runtime::gzip-decompress compressed)))
    (expect (%octet-string decompressed)
            :to-equal (concatenate 'string (%octet-string #(1 12 0 243 255)) plain))))
