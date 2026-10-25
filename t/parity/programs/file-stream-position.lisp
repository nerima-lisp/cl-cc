(progn
  (with-open-file (s "parity-position.txt" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string "abcdef" s))
  (with-open-file (s "parity-position.txt" :direction :input)
    (file-position s 3)
    (write-char (read-char s)))
  0)
