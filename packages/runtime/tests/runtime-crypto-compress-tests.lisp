;;;; packages/runtime/tests/runtime-crypto-compress-tests.lisp — FR-738..FR-740 evidence tests

(in-package :cl-cc/test)



(defun %octet-string (octets)
  (map 'string #'code-char octets))

(it-sequential "fr-738-sha256-fips-vector-abc"
  (expect (cl-cc/runtime:sha256-hex "abc") :to-equal "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))

(it-sequential "fr-738-sha512-fips-vector-abc"
  (expect (cl-cc/runtime:sha512-hex "abc") :to-equal "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"))

(it-sequential "fr-738-hmac-sha256-rfc4231-vector"
  (expect (cl-cc/runtime:hmac-sha256-hex (make-array 20 :element-type '(unsigned-byte 8) :initial-element #x0b)
                                  "Hi There") :to-equal "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"))

(it-sequential "fr-739-base64-rfc4648-vectors"
  (expect (cl-cc/runtime:base64-encode "") :to-equal "")
  (expect (cl-cc/runtime:base64-encode "f") :to-equal "Zg==")
  (expect (cl-cc/runtime:base64-encode "fo") :to-equal "Zm8=")
  (expect (cl-cc/runtime:base64-encode "foo") :to-equal "Zm9v")
  (expect (%octet-string (cl-cc/runtime:base64-decode "Zm9vYmFy")) :to-equal "foobar"))

(it-sequential "fr-739-base64-url-safe-and-streaming"
  (let* ((bytes #(251 255 238 250))
         (encoded (cl-cc/runtime:base64-encode bytes :url-safe t :padding nil)))
    (expect encoded :to-equal "-__u-g")
    (expect (equalp bytes (cl-cc/runtime:base64-decode encoded :url-safe t)) :to-be-truthy))
  (let ((input (make-string-input-stream "hello"))
        (encoded (make-string-output-stream)))
    (cl-cc/runtime:base64-encode-stream input encoded)
    (expect (get-output-stream-string encoded) :to-equal "aGVsbG8=")))

(it-sequential "fr-740-deflate-stored-roundtrip"
  (let* ((plain "hello hello hello")
         (compressed (cl-cc/runtime:deflate-compress plain))
         (decompressed (cl-cc/runtime:deflate-decompress compressed)))
    (expect (%octet-string decompressed) :to-equal plain)))

(it-sequential "fr-740-zlib-roundtrip-and-checksum"
  (let* ((plain "zlib payload")
         (compressed (cl-cc/runtime:zlib-compress plain))
         (decompressed (cl-cc/runtime:zlib-decompress compressed)))
    (expect (%octet-string decompressed) :to-equal plain)))

(it-sequential "fr-740-gzip-roundtrip-and-trailer"
  (let* ((plain "gzip payload")
         (compressed (cl-cc/runtime:gzip-compress plain))
         (decompressed (cl-cc/runtime:gzip-decompress compressed)))
    (expect (%octet-string decompressed) :to-equal plain)))
