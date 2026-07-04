;;;; PHP IO/runtime builtin data tables.

(in-package :cl-cc/php)

(defparameter *php-ini-defaults*
  '(("display_errors" . "1")
    ("error_reporting" . "32767")
    ("html_errors" . "0")
    ("log_errors" . "0")
    ("max_execution_time" . "0")
    ("memory_limit" . "-1")
    ("max_memory_limit" . "-1")
    ("fatal_error_backtraces" . "0")
    ("default_charset" . "UTF-8")
    ("date.timezone" . "UTC")
    ("precision" . "14")
    ("serialize_precision" . "-1"))
  "Default INI values modelled by the PHP runtime.")

(defparameter *php-locale-likely-subtags*
  '(("en" . "en-Latn-US")
    ("ja" . "ja-Jpan-JP")
    ("zh" . "zh-Hans-CN")
    ("fr" . "fr-Latn-FR")
    ("de" . "de-Latn-DE")
    ("ar" . "ar-Arab-EG")
    ("he" . "he-Hebr-IL")
    ("fa" . "fa-Arab-IR")
    ("ur" . "ur-Arab-PK"))
  "Deterministic likely-subtag sample used by PHP 8.5 Locale helpers.")

(defparameter +php-cookie-option-keys+
  '("expires" "path" "domain" "secure" "httponly" "samesite" "partitioned"))

(defparameter +php-session-cookie-option-keys+
  '("lifetime" "path" "domain" "secure" "partitioned" "httponly" "samesite"))

(defparameter +php-tokenizer-token-names+
  '("T_LNUMBER"
    "T_DNUMBER"
    "T_STRING"
    "T_VARIABLE"
    "T_CONSTANT_ENCAPSED_STRING"
    "T_OBJECT_OPERATOR"
    "T_DOUBLE_ARROW"
    "T_COMMENT"
    "T_DOC_COMMENT"
    "T_OPEN_TAG"
    "T_OPEN_TAG_WITH_ECHO"
    "T_CLOSE_TAG"
    "T_WHITESPACE"
    "T_DOUBLE_COLON"
    "T_VOID_CAST"
    "T_PIPE"
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
    "T_YIELD_FROM"
    "T_ATTRIBUTE"
    "T_NS_SEPARATOR"
    "T_NAME_FULLY_QUALIFIED"
    "T_NAME_QUALIFIED"
    "T_NAME_RELATIVE"
    "T_BAD_CHARACTER"
    "T_LOGICAL_AND"
    "T_LOGICAL_OR"
    "T_LOGICAL_XOR"))
