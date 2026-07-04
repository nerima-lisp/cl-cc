;;;; cl-cc-php.asd — PHP frontend: lexer, parser, grammar

(asdf:defsystem :cl-cc-php
  :description "CL-CC PHP frontend: lexer, parser, and grammar"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-ast :cl-cc-bootstrap :cl-cc-parse :cl-cc-vm)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "lexer")
   (:file "lexer-ops")
    (:file "runtime-helpers")
    ;; PHP runtime builtins (count, array_*, str*, math, type predicates, and the
    ;; dispatch registry). Previously omitted, so their bodies were never compiled
    ;; — runtime calls such as array_find -> %php-array-pairs / %php-callable-function
    ;; hit undefined functions. register loads last (it references the others).
    (:file "runtime-builtins-core")
    (:file "runtime-builtins-array")
    (:file "runtime-builtins-string-data")
    (:file "runtime-builtins-string")
    (:file "runtime-builtins-string-core")
    (:file "runtime-builtins-string-format")
    (:file "runtime-builtins-string-multibyte")
    (:file "runtime-builtins-string-encoding")
    (:file "runtime-builtins-string-transform")
    (:file "runtime-builtins-string-analysis")
    (:file "runtime-builtins-string-ctype")
    (:file "runtime-builtins-string-extra")
    (:file "runtime-builtins-string-serialization")
    (:file "runtime-builtins-string-json")
    (:file "runtime-builtins-string-digest")
     (:file "runtime-builtins-regex")
     (:file "runtime-builtins-math")
     (:file "runtime-builtins-types")
     (:file "runtime-builtins-io-data")
     (:file "runtime-builtins-io")
     (:file "runtime-builtins-io-files")
     (:file "runtime-builtins-io-objects")
     (:file "runtime-builtins-io-spl")
     (:file "runtime-builtins-io-reflection-objects")
     (:file "runtime-builtins-io-compat-objects")
     (:file "runtime-builtins-io-image")
     (:file "runtime-builtins-io-output")
     (:file "runtime-builtins-io-cookie-session")
     (:file "runtime-builtins-io-tokenizer")
     (:file "runtime-builtins-io-uri")
     (:file "runtime-builtins-register")
     (:file "parser")
     (:file "parser-support")
     (:file "parser-attributes")
     (:file "parser-expr")
     (:file "parser-expr-advanced")
   (:file "parser-stmt-lowering")
   (:file "parser-stmt-decls")
    (:file "parser-class")
    (:file "parser-trait")
    (:file "parser-interface")
    (:file "php84-features")
    (:file "unsupported")
    (:file "grammar")
   (:file "grammar-stmt")))
