;;;; packages/vm/tests/regex-tests.lisp — FR-672 regex engine tests

(in-package :cl-cc/test)



(it-sequential "regex-scan-literals-and-quantifiers"
  (let* ((text "xx abbbcXYZ yy")
         (match (cl-regex-kit:scan (cl-regex-kit:compile-regex "ab+c.{2,3}") text)))
    (expect match :to-be-truthy)
    (expect (cl-regex-kit:match-start match) :to-equal 3)
    (expect (cl-regex-kit:match-string match text) :to-equal "abbbcXYZ"))
  (let ((text "color"))
    (expect (cl-regex-kit:match-string
             (cl-regex-kit:scan (cl-regex-kit:compile-regex "colou?r") text)
             text)
            :to-equal "color"))
  (let ((text "colour"))
    (expect (cl-regex-kit:match-string
             (cl-regex-kit:scan (cl-regex-kit:compile-regex "colou?r") text)
             text)
            :to-equal "colour")))

(it-sequential "regex-scan-classes-escapes-and-anchors"
  (let ((text "abc123"))
    (expect (cl-regex-kit:match-string
             (cl-regex-kit:scan (cl-regex-kit:compile-regex "^[a-z]+\\d+$") text)
             text)
            :to-equal "abc123"))
  (let ((text "!foo_bar!"))
    (expect (cl-regex-kit:match-string
             (cl-regex-kit:scan (cl-regex-kit:compile-regex "\\w+") text)
             text)
            :to-equal "foo_bar"))
  (let ((text (concatenate 'string "x " (string #\Tab) " y")))
    (expect (cl-regex-kit:match-string
             (cl-regex-kit:scan (cl-regex-kit:compile-regex "\\s+") text)
             text)
            :to-equal (concatenate 'string " " (string #\Tab) " ")))
  (let ((text "123xy9"))
    (expect (cl-regex-kit:match-string
             (cl-regex-kit:scan (cl-regex-kit:compile-regex "[^0-9]+") text)
             text)
            :to-equal "xy")))

(it-sequential "regex-all-matches"
  (let ((text "a1 b22 c333"))
    (expect (mapcar (lambda (match)
                      (cl-regex-kit:match-string match text))
                    (cl-regex-kit:all-matches
                     (cl-regex-kit:compile-regex "[a-z]\\d+")
                     text))
            :to-equal '("a1" "b22" "c333"))))

(it-sequential "regex-capture-groups"
  (let* ((text "id abc-123 done")
         (match (cl-regex-kit:scan
                 (cl-regex-kit:compile-regex "([a-z]+)-(\\d+)")
                 text)))
    (expect match :to-be-truthy)
    (expect (cl-regex-kit:match-group-string match 0 text) :to-equal "abc-123")
    (expect (cl-regex-kit:match-group-string match 1 text) :to-equal "abc")
    (expect (cl-regex-kit:match-group-string match 2 text) :to-equal "123")
    (expect (length (cl-regex-kit:match-captures match text)) :to-equal 3)))

(it-sequential "regex-replace-first-and-all"
  (expect (cl-regex-kit:replace-first
           (cl-regex-kit:compile-regex "([a-z]+)")
           "x abc y def"
           "<$1>"
           :start 2)
          :to-equal "x <abc> y def")
  (expect (cl-regex-kit:replace-all
           (cl-regex-kit:compile-regex "\\d+")
           "1 22 333"
           "#")
          :to-equal "# # #"))

(it-sequential "regex-compiler-builds-public-regex"
  (let ((compiled (cl-regex-kit:compile-regex "(a+)b")))
    (expect (cl-regex-kit:regex-p compiled) :to-be-truthy)
    (expect (cl-regex-kit:regex-source compiled) :to-equal "(a+)b")
    (expect (cl-regex-kit:regex-group-count compiled) :to-equal 1)
    (expect (cl-regex-kit:regex-capture-count compiled) :to-equal 2)))

(it-sequential "regex-unicode-decimal-and-word-classes"
  (let ((arabic-three (string (code-char #x0663)))
        (connector (string (code-char #x203F))))
    (let ((text (concatenate 'string "x" arabic-three "y")))
      (expect (cl-regex-kit:match-string
               (cl-regex-kit:scan (cl-regex-kit:compile-regex "\\d+") text)
               text)
              :to-equal arabic-three))
    (let ((text (concatenate 'string "!λ" connector "9!")))
      (expect (cl-regex-kit:match-string
               (cl-regex-kit:scan (cl-regex-kit:compile-regex "\\w+") text)
               text)
              :to-equal (concatenate 'string "λ" connector "9")))))

(it-sequential "regex-unicode-property-escapes"
  (let ((text "1λ!"))
    (expect (cl-regex-kit:match-string
             (cl-regex-kit:scan (cl-regex-kit:compile-regex "\\p{Letter}+") text)
             text)
            :to-equal "λ"))
  (let ((text "a_b"))
    (expect (cl-regex-kit:match-string
             (cl-regex-kit:scan
              (cl-regex-kit:compile-regex "\\p{Connector_Punctuation}") text)
             text)
            :to-equal "_"))
  (let ((text "x3y"))
    (expect (cl-regex-kit:match-string
             (cl-regex-kit:scan (cl-regex-kit:compile-regex "\\p{Nd}+") text)
             text)
            :to-equal "3")))

(it-sequential "regex-unicode-case-insensitive"
  (let ((text "xxÉyy"))
    (expect (cl-regex-kit:match-string
             (cl-regex-kit:scan (cl-regex-kit:compile-regex "(?i)é") text)
             text)
            :to-equal "É"))
  (let ((text "αλω"))
    (expect (cl-regex-kit:match-string
             (cl-regex-kit:scan (cl-regex-kit:compile-regex "(?i)Λ") text)
             text)
            :to-equal "λ")))
