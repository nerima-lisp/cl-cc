(defpackage :cl-cc
  (:use :cl :cl-cc/bootstrap :cl-cc/ast :cl-cc/parse :cl-cc/optimize :cl-cc/emit :cl-cc/expand :cl-cc/compile :cl-cc/vm :cl-cc/stdlib :cl-cc/pipeline :cl-cc/selfhost :cl-cc/repl)
  (:shadowing-import-from :cl-cc/expand
    #:define-syntax
)

  (:shadowing-import-from :cl-cc/vm
    #:get-universal-time #:get-internal-real-time #:get-internal-run-time
    #:internal-time-units-per-second #:sleep #:time
    #:encode-universal-time #:decode-universal-time
    #:random-state #:random-state-p #:make-random-state #:*random-state* #:random
    #:*print-base* #:*print-radix* #:*print-circle*
    #:*print-pretty* #:*print-level* #:*print-length*
    #:*print-readably* #:*print-pprint-dispatch*
    #:with-standard-io-syntax
    #:pprint-logical-block #:pprint-indent #:pprint-newline #:pprint-tab
    #:copy-pprint-dispatch #:set-pprint-dispatch #:get-pprint-dispatch
    #:*readtable* #:copy-readtable
    #:set-macro-character #:get-macro-character
    #:set-dispatch-macro-character #:get-dispatch-macro-character
    #:readtable-case
    #:lisp-implementation-type #:lisp-implementation-version
    #:machine-type #:machine-version #:machine-instance
    #:software-type #:software-version
    #:room #:apropos #:apropos-list
    #:*read-base* #:*print-right-margin* #:*print-lines*
    #:*print-case* #:*print-escape* #:*print-gensym* #:*print-array*
    #:*trace-output*
    #:sequence #:elt #:length #:subseq
    #:stream-external-format
    #:break #:describe #:inspect #:ed #:dribble
    #:invoke-debugger #:trace #:untrace #:step
    #:*read-suppress* #:*read-default-float-format* #:*read-eval*
    #:*features* #:*modules*
    #:method-qualifiers #:compute-applicable-methods
    #:find-method #:add-method #:remove-method #:ensure-generic-function
    #:open #:*load-pathname* #:*load-truename* #:*compile-file-pathname*
    #:*compile-file-truename* #:*compile-print* #:*compile-verbose*
    #:*load-print* #:*load-verbose*
    #:exit
    #:getenv #:setenv
    #:*command-line-args* #:*default-external-format*
    #:read-preserving-whitespace #:read-delimited-list
    #:set-syntax-from-char
    #:vm-type-of
))

(in-package :cl-cc)
(setf cl-cc/vm:*vm-guest-function-package* :cl-cc)

(defun gc-stats (&optional heap)
  "Return CL-CC runtime GC statistics as a plist.

Stable public keys include :MINOR-GCS, :MAJOR-GCS,
:TOTAL-COLLECTED-BYTES, and :PAUSE-MS-P99."
  (cl-cc/runtime:gc-stats heap))

(export 'gc-stats :cl-cc)

;; Re-export child-package symbols from the :cl-cc umbrella facade.
#-cl-cc-self-hosting
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Re-export bootstrap symbols (our-eval, binop, make-cst-token, etc.)
  ;; so cl-cc:our-eval etc. continue to work for downstream consumers.
  (do-external-symbols (s :cl-cc/bootstrap)
    (export s :cl-cc))
  ;; Re-export AST symbols from :cl-cc so cl-cc:SYMBOL syntax works
  ;; in downstream packages (e.g. :cl-cc/cli, tests, compile/).
  (do-external-symbols (s :cl-cc/ast)
    (export s :cl-cc))
  ;; Re-export optimize symbols from :cl-cc.
  (do-external-symbols (s :cl-cc/optimize)
    (export s :cl-cc))
  ;; Re-export emit symbols from :cl-cc.
  (do-external-symbols (s :cl-cc/emit)
    (export s :cl-cc))
  ;; Re-export parse symbols from :cl-cc.
  (do-external-symbols (s :cl-cc/parse)
    (export s :cl-cc))
  ;; Re-export expand symbols from :cl-cc.
  (do-external-symbols (s :cl-cc/expand)
    (export s :cl-cc))
  ;; Re-export compile symbols from :cl-cc.
  (do-external-symbols (s :cl-cc/compile)
    (export s :cl-cc))
  ;; Re-export VM symbols from :cl-cc.
  (do-external-symbols (s :cl-cc/vm)
    (export s :cl-cc))
  ;; Re-export stdlib symbols from :cl-cc.
  (do-external-symbols (s :cl-cc/stdlib)
    (export s :cl-cc))
  ;; Re-export pipeline/selfhost/repl symbols from :cl-cc.
  (do-external-symbols (s :cl-cc/pipeline)
    (export s :cl-cc))
  (do-external-symbols (s :cl-cc/selfhost)
    (export s :cl-cc))
  (do-external-symbols (s :cl-cc/repl)
    (export s :cl-cc)))
