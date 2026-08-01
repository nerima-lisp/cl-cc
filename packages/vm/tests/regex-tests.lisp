;;;; packages/vm/tests/regex-tests.lisp — FR-672 regex engine tests

(in-package :cl-cc/test)



(it-sequential "regex-scan-literals-and-quantifiers"
  (let ((match (regex:scan "ab+c.{2,3}" "xx abbbcXYZ yy")))
    (expect match :to-be-truthy)
    (expect (regex:match-start match) :to-equal 3)
    (expect (regex:match-string match) :to-equal "abbbcXYZ"))
  (expect (regex:match-string (regex:scan "colou?r" "color")) :to-equal "color")
  (expect (regex:match-string (regex:scan "colou?r" "colour")) :to-equal "colour"))

(it-sequential "regex-scan-classes-escapes-and-anchors"
  (expect (regex:match-string (regex:scan "^[a-z]+\\d+$" "abc123")) :to-equal "abc123")
  (expect (regex:match-string (regex:scan "\\w+" "!foo_bar!")) :to-equal "foo_bar")
  (expect (regex:match-string
                 (regex:scan "\\s+"
                             (concatenate 'string "x " (string #\Tab) " y"))) :to-equal (concatenate 'string " " (string #\Tab) " "))
  (expect (regex:match-string (regex:scan "[^0-9]+" "123xy9")) :to-equal "xy"))

(it-sequential "regex-all-matches"
  (expect (mapcar #'regex:match-string
                        (regex:all-matches "[a-z]\\d+" "a1 b22 c333")) :to-equal '("a1" "b22" "c333")))

(it-sequential "regex-capture-groups"
  (let ((match (regex:scan "([a-z]+)-(\\d+)" "id abc-123 done")))
    (expect match :to-be-truthy)
    (expect (regex:match-group match 0) :to-equal "abc-123")
    (expect (regex:match-group match 1) :to-equal "abc")
    (expect (regex:match-group match 2) :to-equal "123")
    (expect (length (regex:match-groups match)) :to-equal 3)))

(it-sequential "regex-replace-first-and-all"
  (expect (regex:regex-replace "([a-z]+)" "<\\1>" "x abc y def" :start 2) :to-equal "x <abc> y def")
  (expect (regex:regex-replace-all "\\d+" "#" "1 22 333") :to-equal "# # #"))

(it-sequential "regex-compiler-builds-nfa-and-dfa"
  (let ((compiled (regex:compile "a+b")))
    (expect (regex:compiled-regex-pattern compiled) :to-equal "a+b")
    (expect (regex:compiled-regex-ast compiled) :to-be-truthy)
    (expect (regex:compiled-regex-nfa compiled) :to-be-truthy)
    (expect (regex:compiled-regex-dfa compiled) :to-be-truthy)))

(it-sequential "regex-unicode-decimal-and-word-classes"
  (let ((arabic-three (string (code-char #x0663)))
        (connector (string (code-char #x203F))))
    (expect (regex:match-string (regex:scan "\\d+" (concatenate 'string "x" arabic-three "y"))) :to-equal arabic-three)
    (expect (regex:match-string
                   (regex:scan "\\w+" (concatenate 'string "!λ" connector "9!"))) :to-equal (concatenate 'string "λ" connector "9"))))

(it-sequential "regex-unicode-property-escapes"
  (expect (regex:match-string (regex:scan "\\p{Letter}+" "1λ!")) :to-equal "λ")
  (expect (regex:match-string (regex:scan "\\p{Connector_Punctuation}" "a_b")) :to-equal "_")
  (expect (regex:match-string (regex:scan "\\p{Nd}+" "x3y")) :to-equal "3"))

(it-sequential "regex-unicode-case-insensitive"
  (expect (regex:match-string (regex:scan "(?i)é" "xxÉyy")) :to-equal "É")
  (expect (regex:match-string (regex:scan "(?i)Λ" "αλω")) :to-equal "λ"))
