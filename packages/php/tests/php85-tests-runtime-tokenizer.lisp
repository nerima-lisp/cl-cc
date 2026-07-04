(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(%php85-register-test 'php85-image-type-constants-match-current-values
  "PHP 8.5 IMAGETYPE_* constants match current extension values."
  (lambda ()
    (assert-string= "0:18:19:20:21:22"
                    (%php-run-capture
                     "<?php echo IMAGETYPE_UNKNOWN . ':' . IMAGETYPE_WEBP . ':' . IMAGETYPE_AVIF . ':' . IMAGETYPE_HEIF . ':' . IMAGETYPE_SVG . ':' . IMAGETYPE_COUNT;"))))

(%php85-register-test 'php85-image-type-functions-support-svg
  "PHP 8.5 image type helpers support current IMAGETYPE_* values."
  (lambda ()
    (assert-string= ".svg:svg:image/svg+xml:.webp:webp:image/webp:.avif:avif:image/avif:.heif:heif:image/heif:Y:Y:false"
                    (%php-run-capture
                     "<?php
echo image_type_to_extension(IMAGETYPE_SVG) . ':' .
     image_type_to_extension(IMAGETYPE_SVG, false) . ':' .
     image_type_to_mime_type(IMAGETYPE_SVG) . ':' .
     image_type_to_extension(IMAGETYPE_WEBP) . ':' .
     image_type_to_extension(IMAGETYPE_WEBP, false) . ':' .
     image_type_to_mime_type(IMAGETYPE_WEBP) . ':' .
     image_type_to_extension(IMAGETYPE_AVIF) . ':' .
     image_type_to_extension(IMAGETYPE_AVIF, false) . ':' .
     image_type_to_mime_type(IMAGETYPE_AVIF) . ':' .
     image_type_to_extension(IMAGETYPE_HEIF) . ':' .
     image_type_to_extension(IMAGETYPE_HEIF, false) . ':' .
     image_type_to_mime_type(IMAGETYPE_HEIF) . ':' .
     (function_exists('image_type_to_extension') ? 'Y' : 'N') . ':' .
     (function_exists('image_type_to_mime_type') ? 'Y' : 'N') . ':' .
     (image_type_to_extension(9999) === false ? 'false' : 'other');
"))))

(%php85-register-test 'php85-getimagesize-svg-reports-units-and-exif-type
  "PHP 8.5 getimagesize returns SVG dimensions, units, MIME, and image type."
  (lambda ()
    (assert-string= "12:34:type:image/svg+xml:cm:px:exif:false:Y:Y"
                    (%php-run-capture
                     "<?php
$f = tempnam(sys_get_temp_dir(), 'clcc-svg-');
file_put_contents($f, '<svg width=\"12cm\" height=\"34px\" xmlns=\"http://www.w3.org/2000/svg\"></svg>');
$size = getimagesize($f);
echo $size[0] . ':' .
     $size[1] . ':' .
     ($size[2] === IMAGETYPE_SVG ? 'type' : 'bad') . ':' .
     $size['mime'] . ':' .
     $size['width_unit'] . ':' .
     $size['height_unit'] . ':' .
     (exif_imagetype($f) === IMAGETYPE_SVG ? 'exif' : 'bad') . ':' .
     (getimagesize($f . '.missing') === false ? 'false' : 'other') . ':' .
     (function_exists('getimagesize') ? 'Y' : 'N') . ':' .
     (function_exists('exif_imagetype') ? 'Y' : 'N');
unlink($f);
"))))

(%php85-register-test 'php85-token-name-reports-tokenizer-constants
  "PHP 8.5 tokenizer constants have token_name mappings."
  (lambda ()
    (dolist (name '("T_PIPE"
                    "T_VOID_CAST"
                    "T_CLOSE_TAG"
                    "T_INLINE_HTML"
                    "T_ECHO"
                    "T_CLASS"
                    "T_CONST"
                    "T_PUBLIC"
                    "T_FUNCTION"
                    "T_ABSTRACT"
                    "T_ARRAY"
                    "T_AS"
                    "T_BREAK"
                    "T_CALLABLE"
                    "T_CASE"
                    "T_CATCH"
                    "T_CLONE"
                    "T_CONTINUE"
                    "T_DECLARE"
                    "T_DEFAULT"
                    "T_DO"
                    "T_ELSE"
                    "T_ELSEIF"
                    "T_EMPTY"
                    "T_ENDDECLARE"
                    "T_ENDFOR"
                    "T_ENDFOREACH"
                    "T_ENDIF"
                    "T_ENDSWITCH"
                    "T_ENDWHILE"
                    "T_ENUM"
                    "T_EVAL"
                    "T_EXIT"
                    "T_EXTENDS"
                    "T_FINAL"
                    "T_FINALLY"
                    "T_FN"
                    "T_FOR"
                    "T_FOREACH"
                    "T_GLOBAL"
                    "T_GOTO"
                    "T_IF"
                    "T_IMPLEMENTS"
                    "T_INCLUDE"
                    "T_INCLUDE_ONCE"
                    "T_INSTANCEOF"
                    "T_INSTEADOF"
                    "T_INTERFACE"
                    "T_ISSET"
                    "T_LIST"
                    "T_MATCH"
                    "T_NAMESPACE"
                    "T_NEW"
                    "T_PRINT"
                    "T_PRIVATE"
                    "T_PROTECTED"
                    "T_READONLY"
                    "T_REQUIRE"
                    "T_REQUIRE_ONCE"
                    "T_RETURN"
                    "T_STATIC"
                    "T_SWITCH"
                    "T_THROW"
                    "T_TRAIT"
                    "T_TRY"
                    "T_UNSET"
                    "T_USE"
                    "T_VAR"
                    "T_WHILE"
                    "T_YIELD"
                    "T_LOGICAL_AND"
                    "T_LOGICAL_OR"
                    "T_LOGICAL_XOR"
                    "T_YIELD_FROM"
                    "T_ATTRIBUTE"
                    "T_NS_SEPARATOR"
                    "T_NAME_FULLY_QUALIFIED"
                    "T_NAME_QUALIFIED"
                    "T_NAME_RELATIVE"
                    "T_BAD_CHARACTER"))
      (multiple-value-bind (token-id found)
          (cl-cc/php::%php-lookup-constant name)
        (assert-true found)
        (assert-string= name (cl-cc/php::%php-token-name token-id))))
    (assert-string= "UNKNOWN" (cl-cc/php::%php-token-name -1))))

(%php85-register-test 'php85-token-get-all-exposes-pipe-and-void-cast
  "PHP 8.5 token_get_all exposes pipe and void-cast tokens."
  (lambda ()
    (multiple-value-bind (pipe-id pipe-found)
        (cl-cc/php::%php-lookup-constant "T_PIPE")
      (assert-true pipe-found)
      (multiple-value-bind (void-id void-found)
          (cl-cc/php::%php-lookup-constant "T_VOID_CAST")
        (assert-true void-found)
        (let* ((tokens (cl-cc/php::%php-token-get-all "<?php $x = (VOID) $y |> strlen;"))
               (entries (cl-cc/php::%php-array-values-list tokens)))
          (flet ((entry-id (entry)
                   (and (hash-table-p entry)
                        (cl-cc/php::%php-array-ref entry 0)))
                 (entry-text (entry)
                   (cl-cc/php::%php-array-ref entry 1))
                 (entry-line (entry)
                   (cl-cc/php::%php-array-ref entry 2)))
            (let ((pipe-token (find-if (lambda (entry)
                                         (eql pipe-id (entry-id entry)))
                                       entries))
                  (void-token (find-if (lambda (entry)
                                         (eql void-id (entry-id entry)))
                                       entries)))
              (assert-true pipe-token)
              (assert-true void-token)
              (assert-string= "|>" (entry-text pipe-token))
              (assert-string= "(VOID)" (entry-text void-token))
              (assert-equal 1 (entry-line pipe-token))
              (assert-equal 1 (entry-line void-token)))))))))

(%php85-register-test 'php85-token-get-all-models-html-boundaries
  "PHP 8.5 token_get_all models inline HTML and PHP close tags."
  (lambda ()
    (multiple-value-bind (inline-id inline-found)
        (cl-cc/php::%php-lookup-constant "T_INLINE_HTML")
      (assert-true inline-found)
      (multiple-value-bind (close-id close-found)
          (cl-cc/php::%php-lookup-constant "T_CLOSE_TAG")
        (assert-true close-found)
        (let* ((tokens (cl-cc/php::%php-token-get-all "hello <?php echo 1; ?> world"))
               (entries (cl-cc/php::%php-array-values-list tokens)))
          (flet ((entry-id (entry)
                   (and (hash-table-p entry)
                        (cl-cc/php::%php-array-ref entry 0)))
                 (entry-text (entry)
                   (cl-cc/php::%php-array-ref entry 1))
                 (entry-line (entry)
                   (cl-cc/php::%php-array-ref entry 2)))
            (let ((inline-tokens (remove-if-not (lambda (entry)
                                                  (eql inline-id (entry-id entry)))
                                                entries))
                  (close-token (find-if (lambda (entry)
                                          (eql close-id (entry-id entry)))
                                        entries)))
              (assert-equal 2 (length inline-tokens))
              (assert-string= "hello " (entry-text (first inline-tokens)))
              (assert-string= " world" (entry-text (second inline-tokens)))
              (assert-true close-token)
              (assert-string= "?>" (entry-text close-token))
              (assert-equal 1 (entry-line close-token)))))))))

(%php85-register-test 'php85-token-get-all-models-open-tags-and-keywords
  "PHP 8.5 token_get_all models open-tag trivia, keywords, and TOKEN_PARSE context."
  (lambda ()
    (labels ((entry-id (entry)
               (and (hash-table-p entry)
                    (cl-cc/php::%php-array-ref entry 0)))
             (entry-text (entry)
               (and (hash-table-p entry)
                    (cl-cc/php::%php-array-ref entry 1)))
             (entry-name (entry)
               (and (hash-table-p entry)
                    (cl-cc/php::%php-token-name (entry-id entry)))))
      (let* ((tokens (cl-cc/php::%php-token-get-all "<?php echo; ?>"))
             (entries (cl-cc/php::%php-array-values-list tokens)))
        (assert-string= "T_OPEN_TAG" (entry-name (first entries)))
        (assert-string= "<?php " (entry-text (first entries)))
        (assert-true (find "T_ECHO" entries :key #'entry-name :test #'string=))
        (assert-true (find "T_CLOSE_TAG" entries :key #'entry-name :test #'string=)))
      (let* ((tokens (cl-cc/php::%php-token-get-all "/* comment */"))
             (entries (cl-cc/php::%php-array-values-list tokens)))
        (assert-equal 1 (length entries))
        (assert-string= "T_INLINE_HTML" (entry-name (first entries)))
        (assert-string= "/* comment */" (entry-text (first entries))))
      (dolist (item '(("abstract" . "T_ABSTRACT")
                      ("array" . "T_ARRAY")
                      ("as" . "T_AS")
                      ("break" . "T_BREAK")
                      ("callable" . "T_CALLABLE")
                      ("case" . "T_CASE")
                      ("catch" . "T_CATCH")
                      ("clone" . "T_CLONE")
                      ("continue" . "T_CONTINUE")
                      ("declare" . "T_DECLARE")
                      ("default" . "T_DEFAULT")
                      ("die" . "T_EXIT")
                      ("do" . "T_DO")
                      ("else" . "T_ELSE")
                      ("elseif" . "T_ELSEIF")
                      ("empty" . "T_EMPTY")
                      ("enddeclare" . "T_ENDDECLARE")
                      ("endfor" . "T_ENDFOR")
                      ("endforeach" . "T_ENDFOREACH")
                      ("endif" . "T_ENDIF")
                      ("endswitch" . "T_ENDSWITCH")
                      ("endwhile" . "T_ENDWHILE")
                      ("enum" . "T_ENUM")
                      ("eval" . "T_EVAL")
                      ("exit" . "T_EXIT")
                      ("extends" . "T_EXTENDS")
                      ("final" . "T_FINAL")
                      ("finally" . "T_FINALLY")
                      ("fn" . "T_FN")
                      ("for" . "T_FOR")
                      ("foreach" . "T_FOREACH")
                      ("function" . "T_FUNCTION")
                      ("global" . "T_GLOBAL")
                      ("goto" . "T_GOTO")
                      ("if" . "T_IF")
                      ("implements" . "T_IMPLEMENTS")
                      ("include" . "T_INCLUDE")
                      ("include_once" . "T_INCLUDE_ONCE")
                      ("instanceof" . "T_INSTANCEOF")
                      ("insteadof" . "T_INSTEADOF")
                      ("interface" . "T_INTERFACE")
                      ("isset" . "T_ISSET")
                      ("list" . "T_LIST")
                      ("match" . "T_MATCH")
                      ("namespace" . "T_NAMESPACE")
                      ("new" . "T_NEW")
                      ("print" . "T_PRINT")
                      ("private" . "T_PRIVATE")
                      ("protected" . "T_PROTECTED")
                      ("public" . "T_PUBLIC")
                      ("readonly" . "T_READONLY")
                      ("require" . "T_REQUIRE")
                      ("require_once" . "T_REQUIRE_ONCE")
                      ("return" . "T_RETURN")
                      ("static" . "T_STATIC")
                      ("switch" . "T_SWITCH")
                      ("throw" . "T_THROW")
                      ("trait" . "T_TRAIT")
                      ("try" . "T_TRY")
                      ("unset" . "T_UNSET")
                      ("use" . "T_USE")
                      ("var" . "T_VAR")
                      ("while" . "T_WHILE")
                      ("yield" . "T_YIELD")
                      ("yield from" . "T_YIELD_FROM")
                      ("#[" . "T_ATTRIBUTE")
                      ("\\Foo\\Bar" . "T_NAME_FULLY_QUALIFIED")
                      ("Foo\\Bar" . "T_NAME_QUALIFIED")
                      ("namespace\\Foo" . "T_NAME_RELATIVE")
                      ("\\" . "T_NS_SEPARATOR")
                      ("and" . "T_LOGICAL_AND")
                      ("or" . "T_LOGICAL_OR")
                      ("xor" . "T_LOGICAL_XOR")))
        (let* ((tokens (cl-cc/php::%php-token-get-all
                        (format nil "<?php ~A" (car item))))
               (entries (cl-cc/php::%php-array-values-list tokens)))
          (assert-true (find (cdr item) entries :key #'entry-name :test #'string=))))
      (let* ((tokens (cl-cc/php::%php-token-get-all
                      (concatenate 'string "<?php " (string (code-char 0)))))
             (entries (cl-cc/php::%php-array-values-list tokens)))
        (assert-true (find "T_BAD_CHARACTER" entries :key #'entry-name :test #'string=)))
      (let* ((source "<?php class A { const PUBLIC = 1; function f() {} }")
             (regular (cl-cc/php::%php-array-values-list
                       (cl-cc/php::%php-token-get-all source)))
             (parsed (cl-cc/php::%php-array-values-list
                      (cl-cc/php::%php-token-get-all source 1))))
        (assert-true (find "T_CLASS" regular :key #'entry-name :test #'string=))
        (assert-true (find "T_CONST" regular :key #'entry-name :test #'string=))
        (assert-true (find "T_PUBLIC" regular :key #'entry-name :test #'string=))
        (assert-true (find "T_FUNCTION" regular :key #'entry-name :test #'string=))
        (assert-false (find "T_PUBLIC" parsed :key #'entry-name :test #'string=))
        (assert-true (find-if (lambda (entry)
                                (and (string= "T_STRING" (entry-name entry))
                                     (string= "PUBLIC" (entry-text entry))))
                              parsed))))))

(%php85-register-test 'php85-tokenizer-builtins-execute-from-php-source
  "PHP 8.5 tokenizer builtins are callable from PHP code."
  (lambda ()
    (assert-string= "T_PIPE:T_VOID_CAST:T_INLINE_HTML:T_CLOSE_TAG:T_YIELD_FROM:T_ATTRIBUTE:T_NAME_FULLY_QUALIFIED:T_NAME_QUALIFIED:T_NAME_RELATIVE:T_NS_SEPARATOR:T_BAD_CHARACTER"
                    (%php-run-capture
                     "<?php echo token_name(T_PIPE) . ':' . token_name(T_VOID_CAST) . ':' . token_name(T_INLINE_HTML) . ':' . token_name(T_CLOSE_TAG) . ':' . token_name(T_YIELD_FROM) . ':' . token_name(T_ATTRIBUTE) . ':' . token_name(T_NAME_FULLY_QUALIFIED) . ':' . token_name(T_NAME_QUALIFIED) . ':' . token_name(T_NAME_RELATIVE) . ':' . token_name(T_NS_SEPARATOR) . ':' . token_name(T_BAD_CHARACTER);"))))

(%php85-register-test 'php85-extension-new-free-functions-are-registered
  "New PHP 8.5 extension-level free functions are registered as builtins."
  (lambda ()
    (dolist (function-name '("enchant_dict_remove_from_session"
                             "enchant_dict_remove"
                             "pg_close_stmt"
                             "pg_service"))
      (assert-true (cl-cc/php::%php-function-exists function-name)))))

(%php85-register-test 'php85-cookie-and-session-builtins-are-registered
  "Cookie and session helpers affected by PHP 8.5 are registered as builtins."
  (lambda ()
    (dolist (function-name '("setcookie"
                             "setrawcookie"
                             "session_name"
                             "session_id"
                             "session_set_cookie_params"
                             "session_get_cookie_params"
                             "session_start"))
      (assert-true (cl-cc/php::%php-function-exists function-name)))))

(%php85-register-test 'php85-extension-new-free-function-helpers-update-modeled-state
  "PHP 8.5 enchant and pgsql compatibility helpers update modeled runtime state."
  (lambda ()
    (let ((dict (make-hash-table :test #'equal))
          (words (make-hash-table :test #'equal)))
      (setf (gethash "words" dict) words
            (gethash "hello" words) t)
      (assert-true (cl-cc/php:%php-enchant-dict-remove dict "hello"))
      (assert-false (gethash "hello" words)))
    (let ((connection (make-hash-table :test #'equal))
          (statements (make-hash-table :test #'equal)))
      (setf (gethash "statements" connection) statements
            (gethash "stmt1" statements) t
            (gethash "service" connection) "analytics")
      (assert-string= "analytics" (cl-cc/php:%php-pg-service connection))
      (assert-true (cl-cc/php:%php-pg-close-stmt connection "stmt1"))
      (assert-false (gethash "stmt1" statements)))))

(%php85-register-test 'php85-setcookie-partitioned-option-queues-set-cookie-header
  "PHP 8.5 setcookie() accepts partitioned cookies when secure is enabled."
  (lambda ()
    (let ((cl-cc/php::*php-http-response-code* 200)
          (cl-cc/php::*php-http-headers* nil)
          (cl-cc/php::*php-output-started-p* nil))
      (assert-string= "[\"Set-Cookie: chip=a+b; secure; Partitioned\"]"
                      (%php-run-capture
                       "<?php
setcookie('chip', 'a b', ['secure' => true, 'partitioned' => true]);
echo json_encode(headers_list());")))))

(%php85-register-test 'php85-setcookie-options-are-case-insensitive
  "PHP 8.5 setcookie() option keys are case-insensitive."
  (lambda ()
    (let ((cl-cc/php::*php-http-response-code* 200)
          (cl-cc/php::*php-http-headers* nil)
          (cl-cc/php::*php-output-started-p* nil))
      (assert-string= "[\"Set-Cookie: chip=v; secure; SameSite=Strict; Partitioned\"]"
                      (%php-run-capture
                       "<?php
setcookie('chip', 'v', ['Secure' => true, 'SameSite' => 'Strict', 'Partitioned' => true]);
echo json_encode(headers_list());")))))

(%php85-register-test 'php85-setrawcookie-partitioned-option-queues-set-cookie-header
  "PHP 8.5 setrawcookie() keeps raw values and supports partitioned cookies."
  (lambda ()
    (let ((cl-cc/php::*php-http-response-code* 200)
          (cl-cc/php::*php-http-headers* nil)
          (cl-cc/php::*php-output-started-p* nil))
      (assert-string= "[\"Set-Cookie: raw=a b; secure; SameSite=None; Partitioned\"]"
                      (%php-run-capture
                       "<?php
setrawcookie('raw', 'a b', ['secure' => true, 'samesite' => 'None', 'partitioned' => true]);
echo json_encode(headers_list());")))))

(%php85-register-test 'php85-cookie-partitioned-requires-secure
  "PHP 8.5 partitioned cookies require secure cookies."
  (lambda ()
    (let ((condition (handler-case
                         (progn
                           (cl-cc/php::%php-setcookie
                            "chip"
                            "v"
                            (cl-cc/php:%php-array
                             (list t "partitioned" t)))
                           nil)
                       (cl-cc/php:php-exception (e) e))))
      (assert-true condition)
      (assert-true
       (cl-cc/php:%php-exception-matches-p condition 'value-error)))))

(%php85-register-test 'php85-cookie-option-validation-matches-php85
  "PHP 8.5 setcookie() rejects numeric keys, unknown keys, and invalid SameSite values."
  (lambda ()
    (flet ((raises-value-error-p (thunk)
             (let ((condition (handler-case
                                  (progn (funcall thunk) nil)
                                (cl-cc/php:php-exception (e) e))))
               (and condition
                    (cl-cc/php:%php-exception-matches-p condition 'value-error)))))
      (let ((numeric-options (cl-cc/php::%php-make-array))
            (unknown-options (cl-cc/php::%php-make-array))
            (bad-samesite-options (cl-cc/php::%php-make-array)))
        (cl-cc/php::%php-array-set numeric-options 0 t)
        (cl-cc/php::%php-array-set unknown-options "bogus" t)
        (cl-cc/php::%php-array-set bad-samesite-options "samesite" "Relaxed")
        (assert-true
         (raises-value-error-p
          (lambda () (cl-cc/php::%php-setcookie "chip" "v" numeric-options))))
        (assert-true
         (raises-value-error-p
          (lambda () (cl-cc/php::%php-setcookie "chip" "v" unknown-options))))
        (assert-true
         (raises-value-error-p
          (lambda () (cl-cc/php::%php-setcookie "chip" "v" bad-samesite-options))))))))

(%php85-register-test 'php85-session-cookie-params-support-partitioned
  "PHP 8.5 session cookie params expose and emit partitioned cookies."
  (lambda ()
    (let ((cl-cc/php::*php-http-response-code* 200)
          (cl-cc/php::*php-http-headers* nil)
          (cl-cc/php::*php-output-started-p* nil)
          (cl-cc/php::*php-session-cookie-params* nil)
          (cl-cc/php::*php-session-id* "")
          (cl-cc/php::*php-session-name* "PHPSESSID")
          (cl-cc/php::*php-session-active-p* nil))
      (assert-string= "Y:true:[\"Set-Cookie: PHPSESSID=12345; path=/; secure; Partitioned\"]"
                      (%php-run-capture
                       "<?php
$registered = function_exists('session_set_cookie_params') && function_exists('session_get_cookie_params') && function_exists('session_start') ? 'Y' : 'N';
session_id('12345');
session_set_cookie_params(['secure' => true, 'partitioned' => true]);
$params = session_get_cookie_params();
$partitioned = $params['partitioned'] ? 'true' : 'false';
session_start();
echo $registered . ':' . $partitioned . ':' . json_encode(headers_list());")))))

(%php85-register-test 'php85-mail-function-is-registered-and-returns-true
  "PHP 8.5 mail() is registered and accepts mail requests in the CLI model."
  (lambda ()
    (assert-string= "Y:true"
                    (%php-run-capture
                     "<?php
$registered = function_exists('mail') ? 'Y' : 'N';
$result = mail('to@example.com', 'subject', 'message', ['X-Test: value'], '-f bounce@example.com');
echo $registered . ':' . ($result ? 'true' : 'false');"))))

(%php85-register-test 'php85-session-cookie-param-options-are-case-insensitive
  "PHP 8.5 session cookie param option keys are case-insensitive."
  (lambda ()
    (let ((cl-cc/php::*php-session-cookie-params* nil)
          (cl-cc/php::*php-session-active-p* nil))
      (assert-string= "true:true"
                      (%php-run-capture
                       "<?php
session_set_cookie_params(['Secure' => true, 'Partitioned' => true]);
$params = session_get_cookie_params();
echo ($params['secure'] ? 'true' : 'false') . ':' . ($params['partitioned'] ? 'true' : 'false');")))))

(%php85-register-test 'php85-session-cookie-params-order-includes-partitioned-after-secure
  "PHP 8.5 session_get_cookie_params() inserts partitioned after secure."
  (lambda ()
    (let ((cl-cc/php::*php-session-cookie-params* nil))
      (assert-string=
       "lifetime,path,domain,secure,partitioned,httponly,samesite"
       (format nil "~{~A~^,~}"
               (cl-cc/php::%php-array-ordered-keys
                (cl-cc/php::%php-session-get-cookie-params)))))))

(%php85-register-test 'php85-session-start-cookie-partitioned-requires-secure
  "PHP 8.5 session_start() warns and fails when cookie_partitioned lacks cookie_secure."
  (lambda ()
    (let ((cl-cc/php::*php-http-response-code* 200)
          (cl-cc/php::*php-http-headers* nil)
          (cl-cc/php::*php-output-started-p* nil)
          (cl-cc/php::*php-session-cookie-params* nil)
          (cl-cc/php::*php-session-id* "")
          (cl-cc/php::*php-session-name* "PHPSESSID")
          (cl-cc/php::*php-session-active-p* nil)
          (cl-cc/php::*php-error-handler-stack* nil))
      (assert-string= "2:false:[]"
                      (%php-run-capture
                       "<?php
function php85_session_warning($errno, $errstr, $file, $line) { echo $errno . ':'; return true; }
set_error_handler('php85_session_warning', E_WARNING);
session_id('12345');
session_set_cookie_params(['partitioned' => true]);
$ok = session_start();
restore_error_handler();
echo ($ok ? 'true' : 'false') . ':' . json_encode(headers_list());")))))

(%php85-register-test 'php85-filter-throw-on-failure-constant-is-defined
  "PHP 8.5 defines FILTER_THROW_ON_FAILURE for filter functions."
  (lambda ()
(multiple-value-bind (value found)
      (cl-cc/php::%php-lookup-constant "FILTER_THROW_ON_FAILURE")
    (assert-true found)
    (assert-= 268435456 value))))

(%php85-register-test 'php85-filter-var-validates-int
  "filter_var() supports integer validation."
  (lambda ()
(assert-string= "42:false"
                 (%php-run-capture
                  "<?php echo filter_var('42', FILTER_VALIDATE_INT) . ':' . (filter_var('abc', FILTER_VALIDATE_INT) === false ? 'false' : 'bad');"))))

(%php85-register-test 'php85-filter-var-validates-boolean-false
  "FILTER_VALIDATE_BOOLEAN can return a successful false result."
  (lambda ()
(assert-string= "false"
                 (%php-run-capture
                  "<?php echo filter_var('false', FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE) === false ? 'false' : 'bad';"))))

(%php85-register-test 'php85-filter-var-throws-on-validation-failure
  "FILTER_THROW_ON_FAILURE raises the PHP 8.5 filter exception on validation failure."
  (lambda ()
(assert-signals cl-cc/php:php-exception
    (cl-cc/php:%php-filter-var "abc" 257 268435456))))

(%php85-register-test 'php85-filter-var-throws-filter-failed-exception
  "FILTER_THROW_ON_FAILURE reports the PHP 8.5 Filter\\FilterFailedException class."
  (lambda ()
    (let ((condition (handler-case
                         (progn
                           (cl-cc/php:%php-filter-var "abc" 257 268435456)
                           nil)
                       (cl-cc/php:php-exception (e) e))))
      (assert-true condition)
      (assert-true
       (cl-cc/php:%php-exception-matches-p
        condition
        (intern "FILTER\\FILTERFAILEDEXCEPTION" :cl-cc/php))))))

(%php85-register-test 'php85-filter-var-rejects-null-and-throw-flags-together
  "FILTER_THROW_ON_FAILURE cannot be combined with FILTER_NULL_ON_FAILURE."
  (lambda ()
(assert-signals cl-cc/php:php-exception
    (cl-cc/php:%php-filter-var "42" 257 (+ 134217728 268435456)))))


(eval-when (:load-toplevel :execute)
  (%php85-run-current-source-tests))
