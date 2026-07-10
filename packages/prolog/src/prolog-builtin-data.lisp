(in-package :cl-cc/prolog)

;;; Prolog built-in and DCG dispatch data

(defparameter *builtin-predicate-specs*
  '((and solve-conjunction)
    (or prolog-or-handler)
    (= prolog-unify-handler)
    (/= prolog-not-unify-handler)
    (:when prolog-when-handler))
  "Data table of built-in Prolog predicates and their CPS handlers.")

(defparameter *dcg-sync-tokens* '(:T-RPAREN :T-SEMI :T-EOF)
  "Token types used as synchronization points for DCG error recovery.")

(defparameter *dcg-builtin-specs*
  '((dcg-alt %dcg-alt)
    (dcg-opt %dcg-opt)
    (dcg-star %dcg-star)
    (dcg-plus %dcg-plus)
    (dcg-error-recovery %dcg-error-recovery)
    (dcg-token-match %dcg-token-match)
    (dcg-token-match-value %dcg-token-match-value))
  "DCG builtin predicate registrations.")
