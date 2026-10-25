(progn
  (with-open-file (s "parity-file.txt" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line "file-data" s))
  (with-open-file (s "parity-file.txt" :direction :input)
    (write-string (read-line s)))
  0)
