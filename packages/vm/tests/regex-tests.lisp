;;;; packages/vm/tests/regex-tests.lisp — FR-672 regex engine tests

(in-package :cl-cc/test)



(it-sequential "regex-scan-returns-bounds-and-captures"
  (multiple-value-bind (start end groups)
      (cl-cc/vm:regex-scan "(\\d+)-(\\w+)" "order 42-widget shipped")
    (expect start :to-equal 6)
    (expect end :to-equal 15)
    (expect (aref groups 0) :to-equal "42-widget")
    (expect (aref groups 1) :to-equal "42")
    (expect (aref groups 2) :to-equal "widget")))

(it-sequential "regex-scan-returns-nil-when-not-found"
  (expect (cl-cc/vm:regex-scan "zzz" "abc") :to-be nil))

(it-sequential "regex-all-matches-returns-non-overlapping-ranges"
  (let* ((subject "a1 b22 c333")
         (ranges (cl-cc/vm:regex-all-matches "\\d+" subject)))
    (expect (length ranges) :to-equal 3)
    (expect (mapcar (lambda (range)
                      (subseq subject (car range) (cdr range)))
                    ranges)
            :to-equal '("1" "22" "333"))))

(it-sequential "regex-replace-replaces-only-first-match"
  (expect (cl-cc/vm:regex-replace "\\d+" "id 42 here, id 43 there" "N")
          :to-equal "id N here, id 43 there"))

(it-sequential "regex-replace-all-replaces-every-match"
  (expect (cl-cc/vm:regex-replace-all "\\d+" "a1 b22 c333" "#")
          :to-equal "a# b# c#"))

(it-sequential "regex-split-keeps-empty-fields"
  (expect (cl-cc/vm:regex-split "," "a,b,,c")
          :to-equal '("a" "b" "" "c")))

(it-sequential "regex-scan-honors-inline-case-insensitive-flag"
  (multiple-value-bind (start end)
      (cl-cc/vm:regex-scan "(?i)hello" "say HELLO now")
    (expect start :to-equal 4)
    (expect end :to-equal 9)))

(it-sequential "regex-scan-matches-unicode-letter-properties"
  (multiple-value-bind (start end)
      (cl-cc/vm:regex-scan "\\p{L}+" "  abc123  ")
    (expect start :to-equal 2)
    (expect end :to-equal 5)))
