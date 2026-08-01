;;;; packages/vm/tests/regex-tests.lisp — FR-672 regex engine tests

(in-package :cl-cc/test)



(it-sequential "regex-scan-literals-and-quantifiers"
  (let ((text "xx abbbcXYZ yy")
        (match (cl-regex-kit:scan
                (cl-regex-kit:regex "ab+c.{2,3}")
                "xx abbbcXYZ yy")))
    (expect match :to-be-truthy)
    (expect (cl-regex-kit:match-start match) :to-equal 3)
    (expect (cl-regex-kit:match-string match text) :to-equal "abbbcXYZ"))
  (expect (cl-regex-kit:match-string
           (cl-regex-kit:scan (cl-regex-kit:regex "colou?r") "color")
           "color")
          :to-equal "color")
  (expect (cl-regex-kit:match-string
           (cl-regex-kit:scan (cl-regex-kit:regex "colou?r") "colour")
           "colour")
          :to-equal "colour"))

(it-sequential "regex-scan-classes-escapes-and-anchors"
  (expect (cl-regex-kit:match-string
           (cl-regex-kit:scan (cl-regex-kit:regex "^[a-z]+\\d+$") "abc123")
           "abc123")
          :to-equal "abc123")
  (expect (cl-regex-kit:match-string
           (cl-regex-kit:scan (cl-regex-kit:regex "\\w+") "!foo_bar!")
           "!foo_bar!")
          :to-equal "foo_bar")
  (let ((text (concatenate 'string "x " (string #\Tab) " y")))
    (expect (cl-regex-kit:match-string
             (cl-regex-kit:scan (cl-regex-kit:regex "\\s+") text)
             text)
            :to-equal (concatenate 'string " " (string #\Tab) " ")))
  (expect (cl-regex-kit:match-string
           (cl-regex-kit:scan (cl-regex-kit:regex "[^0-9]+") "123xy9")
           "123xy9")
          :to-equal "xy"))

(it-sequential "regex-all-matches"
  (let ((text "a1 b22 c333"))
    (expect (mapcar (lambda (match)
                      (cl-regex-kit:match-string match text))
                    (cl-regex-kit:all-matches
                     (cl-regex-kit:regex "[a-z]\\d+")
                     text))
            :to-equal '("a1" "b22" "c333"))))

(it-sequential "regex-capture-groups"
  (let ((text "id abc-123 done")
        (match (cl-regex-kit:scan
                (cl-regex-kit:regex "([a-z]+)-(\\d+)")
                "id abc-123 done")))
    (expect match :to-be-truthy)
    (expect (cl-regex-kit:match-group-string match 0 text) :to-equal "abc-123")
    (expect (cl-regex-kit:match-group-string match 1 text) :to-equal "abc")
    (expect (cl-regex-kit:match-group-string match 2 text) :to-equal "123")
    (expect (length (cl-regex-kit:match-captures match text)) :to-equal 3)))

(it-sequential "regex-replace-first-and-all"
  (expect (cl-regex-kit:replace-first
           (cl-regex-kit:regex "([a-z]+)")
           "x abc y def"
           (lambda (match text)
             (format nil "<~A>" (cl-regex-kit:match-group-string match 1 text)))
           :start 2)
          :to-equal "x <abc> y def")
  (expect (cl-regex-kit:replace-all
           (cl-regex-kit:regex "\\d+")
           "1 22 333"
           "#")
          :to-equal "# # #"))

(it-sequential "regex-compiler-builds-regex"
  (let ((compiled (cl-regex-kit:regex "a+b")))
    (expect (cl-regex-kit:regex-source compiled) :to-equal "a+b")
    (expect (cl-regex-kit:regex-p compiled) :to-be-truthy)))

(it-sequential "regex-unicode-decimal-and-word-classes"
  (let ((arabic-three (string (code-char #x0663)))
        (connector (string (code-char #x203F))))
    (let ((decimal-text (concatenate 'string "x" arabic-three "y"))
          (word-text (concatenate 'string "!λ" connector "9!")))
      (expect (cl-regex-kit:match-string
               (cl-regex-kit:scan (cl-regex-kit:regex "\\d+") decimal-text)
               decimal-text)
              :to-equal arabic-three)
      (expect (cl-regex-kit:match-string
               (cl-regex-kit:scan (cl-regex-kit:regex "\\w+") word-text)
               word-text)
              :to-equal (concatenate 'string "λ" connector "9")))))

(it-sequential "regex-unicode-property-escapes"
  (expect (cl-regex-kit:match-string
           (cl-regex-kit:scan (cl-regex-kit:regex "\\p{Letter}+") "1λ!")
           "1λ!")
          :to-equal "λ")
  (expect (cl-regex-kit:match-string
           (cl-regex-kit:scan (cl-regex-kit:regex "\\p{Connector_Punctuation}") "a_b")
           "a_b")
          :to-equal "_")
  (expect (cl-regex-kit:match-string
           (cl-regex-kit:scan (cl-regex-kit:regex "\\p{Nd}+") "x3y")
           "x3y")
          :to-equal "3"))

(it-sequential "regex-unicode-case-insensitive"
  (expect (cl-regex-kit:match-string
           (cl-regex-kit:scan (cl-regex-kit:regex "(?i)é") "xxÉyy")
           "xxÉyy")
          :to-equal "É")
  (expect (cl-regex-kit:match-string
           (cl-regex-kit:scan (cl-regex-kit:regex "(?i)Λ") "αλω")
           "αλω")
          :to-equal "λ"))
