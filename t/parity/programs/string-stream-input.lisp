(progn (write-char (with-input-from-string (s "abc") (read-char s))) 0)
