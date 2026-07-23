;;;; FR-673 keyword argument dispatch optimization tests

(in-package :cl-cc/test)


(it-sequential "fr-673-static-keyword-call-becomes-positional"
  (multiple-value-bind (form reason)
      (cl-cc/expand:keyword-optimize-call
       'target '(x &key (a 10) (b 20)) '(1 :b 2 :a 3))
    (expect reason :to-be :static-keywords)
    (expect form :to-equal '(%keyword-positional-call target 3 1 3 2))))

(it-sequential "fr-673-allow-other-keys-forces-conservative-path"
  (multiple-value-bind (form reason)
      (cl-cc/expand:keyword-optimize-call
       'target '(x &key a &allow-other-keys) '(1 :a 2 :unknown 3))
    (expect reason :to-be :allow-other-keys)
    (expect (null form) :to-be-truthy)))

(it-sequential "fr-673-runtime-bitmask-dispatch-uses-popcount-index"
  (let* ((positions '((:a . 0) (:b . 1) (:c . 2)))
         (args '(:c 30 :a 10))
         (mask (cl-cc/expand:%keyword-call-bitmask args positions))
         (values (cl-cc/expand:%keyword-values-vector args positions)))
    (expect (= 5 mask) :to-be-truthy)
    (expect (= 10 (cl-cc/expand:%keyword-dispatch-value 0 mask values :missing)) :to-be-truthy)
    (expect (cl-cc/expand:%keyword-dispatch-value 1 mask values :missing) :to-be :missing)
    (expect (= 30 (cl-cc/expand:%keyword-dispatch-value 2 mask values :missing)) :to-be-truthy)))
