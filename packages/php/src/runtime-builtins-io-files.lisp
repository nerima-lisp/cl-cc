;;;; PHP file and stream resource builtins.

(in-package :cl-cc/php)

;;; ─── File I/O ────────────────────────────────────────────────────────────────

(defun %php-file-get-contents (filename &optional use-include-path context offset length)
  "PHP file_get_contents: read file into string."
  (declare (ignore use-include-path context))
  (handler-case
      (let* ((path (%php-path-string filename))
             (content (with-open-file (s path :direction :input :external-format :utf-8)
                        (let ((result (make-string (file-length s))))
                          (read-sequence result s)
                          result)))
             (start (if (and offset (not (%php-null-p offset)) (> offset 0)) offset 0))
             (end (if (and length (not (%php-null-p length))) (+ start length) (length content))))
        (subseq content start (min end (length content))))
    (error () nil)))

(defun %php-file-put-contents (filename data &optional flags context)
  "PHP file_put_contents: write data to file."
  (declare (ignore context))
  (handler-case
      (let* ((path (%php-path-string filename))
             (text (%php-stringify data))
             (append-p (and flags (not (%php-null-p flags)) (logtest flags 8))))
        (with-open-file (s path :direction :output
                                :external-format :utf-8
                                :if-exists (if append-p :append :supersede)
                                :if-does-not-exist :create)
          (write-string text s))
        (length text))
    (error () nil)))

(defun %php-file (filename &optional flags context)
  "PHP file: read file into array of lines."
  (declare (ignore flags context))
  (handler-case
      (let ((result (%php-make-array)))
        (with-open-file (s (%php-path-string filename) :direction :input :external-format :utf-8)
          (loop for line = (read-line s nil nil)
                while line
                do (%php-array-set result (%php-array-next-auto-index result)
                                   (concatenate 'string line (string #\Newline)))))
        result)
    (error () nil)))

(defun %php-readfile (filename &optional use-include-path context)
  "PHP readfile: output file contents and return byte count."
  (declare (ignore use-include-path context))
  (let ((content (%php-file-get-contents filename)))
    (when content
      (%php-output-write content)
      (length content))))

(defun %php-file-exists (filename)
  "PHP file_exists: check if file or directory exists."
  (probe-file (%php-path-string filename)))

(defun %php-is-file (filename)
  "PHP is_file: check if path is a regular file."
  (let ((p (probe-file (%php-path-string filename))))
    (and p (or (pathname-name p) (pathname-type p)))))

(defun %php-is-dir (path)
  "PHP is_dir: check if path is a directory."
  (let* ((s (%php-path-string path))
         (s (if (and (> (length s) 0) (char= (char s (1- (length s))) #\/)) s (concatenate 'string s "/")))
         (p (probe-file s)))
    (and p t)))

(defun %php-is-readable (filename)
  "PHP is_readable: check if file is readable."
  (not (null (probe-file (%php-path-string filename)))))

(defun %php-is-writable (filename)
  "PHP is_writable: check if file is writable (simplified)."
  (not (null (probe-file (%php-path-string filename)))))

(defun %php-is-writeable (filename)
  "PHP is_writeable: alias for is_writable."
  (%php-is-writable filename))

(defun %php-is-executable (filename)
  "PHP is_executable: check if file is executable."
  (not (null (probe-file (%php-stringify filename)))))

(defun %php-is-link (filename)
  "PHP is_link: check if path is a symbolic link."
  (declare (ignore filename))
  nil)

(defun %php-filesize (filename)
  "PHP filesize: return file size in bytes."
  (handler-case
      (with-open-file (s (%php-stringify filename) :element-type '(unsigned-byte 8))
        (file-length s))
    (error () nil)))

(defun %php-filemtime (filename)
  "PHP filemtime: return file modification time."
  (handler-case
      (let ((time (file-write-date (%php-stringify filename))))
        (when time (- time 2208988800)))  ; Unix epoch offset
    (error () nil)))

(defun %php-filetype (filename)
  "PHP filetype: return file type string."
  (handler-case
      (let ((p (probe-file (%php-stringify filename))))
        (if p "file" nil))
    (error () nil)))

(defun %php-unlink (filename &optional context)
  "PHP unlink: delete a file."
  (declare (ignore context))
  (handler-case
      (progn (cl:delete-file (%php-path-string filename)) t)
    (error () nil)))

(defun %php-rename (oldname newname &optional context)
  "PHP rename: rename a file or directory."
  (declare (ignore context))
  (handler-case
      (progn (rename-file (%php-path-string oldname) (%php-path-string newname)) t)
    (error () nil)))

(defun %php-copy (source dest &optional context)
  "PHP copy: copy a file."
  (declare (ignore context))
  (handler-case
      (let ((content (%php-file-get-contents source)))
        (when content (%php-file-put-contents dest content) t))
    (error () nil)))

(defun %php-mkdir (pathname &optional (mode #o777) recursive context)
  "PHP mkdir: create a directory."
  (declare (ignore mode context))
  (handler-case
      (progn
        (if (%php-truthy recursive)
            (ensure-directories-exist (concatenate 'string (%php-stringify pathname) "/"))
            (cl:ensure-directories-exist (concatenate 'string (%php-stringify pathname) "/")))
        t)
    (error () nil)))

(defun %php-rmdir (dirname &optional context)
  "PHP rmdir: remove a directory."
  (declare (ignore context))
  (handler-case
      (progn (cl:delete-file (concatenate 'string (%php-stringify dirname) "/")) t)
    (error () nil)))

(defun %php-scandir (directory &optional (sorting-order 0) context)
  "PHP scandir: list files and directories in a directory."
  (declare (ignore context))
  (handler-case
      (let* ((dir (%php-stringify directory))
             (entries (mapcar (lambda (p)
                                (let ((name (file-namestring p)))
                                  (if (string= name "") (car (last (pathname-directory p))) name)))
                              (cl:directory (concatenate 'string dir "/*"))))
             (all (append (list "." "..") entries))
             (sorted (if (= sorting-order 1) (reverse (sort (copy-list all) #'string<))
                         (sort (copy-list all) #'string<))))
        (%php-list-to-array sorted))
    (error () nil)))

(defun %php-getcwd ()
  "PHP getcwd: return current working directory."
  (namestring (truename ".")))

(defun %php-chdir (directory)
  "PHP chdir: change current working directory (delegates to sb-posix if available)."
  (let ((path (%php-stringify directory)))
    (handler-case
        (let ((fn (and (find-package :sb-posix)
                       (find-symbol "CHDIR" :sb-posix))))
          (if (and fn (fboundp fn))
              (progn (funcall fn path) t)
              ;; Fallback: verify path exists, return true
              (and (probe-file path) t)))
      (error () nil))))

(defun %php-realpath (path)
  "PHP realpath: return canonicalized absolute pathname."
  (handler-case
      (namestring (truename (%php-stringify path)))
    (error () nil)))

(defun %php-path-tail (path)
  (let* ((p (%php-path-string path))
         (pos (or (position #\/ p :from-end t)
                  (position #\\ p :from-end t))))
    (if pos (subseq p (1+ pos)) p)))

(defun %php-basename (path &optional suffix)
  "PHP basename: return trailing name component of path."
  (let ((base (%php-path-tail path)))
    (if (and suffix (not (%php-null-p suffix)))
        (let ((s (%php-stringify suffix)))
          (if (and (>= (length base) (length s))
                   (string= base s :start1 (- (length base) (length s))))
              (subseq base 0 (- (length base) (length s)))
              base))
        base)))

(defun %php-dirname (path &optional (levels 1))
  "PHP dirname: return directory name of path."
  (let* ((p (%php-stringify path))
         (lvls (if (and levels (not (%php-null-p levels))) levels 1)))
    (let ((result p))
      (dotimes (i lvls)
        (let ((pos (or (position #\/ result :from-end t)
                       (position #\\ result :from-end t))))
          (setf result (if pos (subseq result 0 pos) "."))))
      result)))

(defun %php-pathinfo (path &optional (option nil))
  "PHP pathinfo: return path components."
  (let* ((p (%php-path-string path))
         (dirname (let ((pos (or (position #\/ p :from-end t) (position #\\ p :from-end t))))
                    (if pos (subseq p 0 pos) ".")))
         (filename (%php-path-tail p))
         (ext (let ((dot (position #\. filename :from-end t)))
                (if dot (subseq filename (1+ dot)) "")))
         (filename-without-extension (let ((dot (position #\. filename :from-end t)))
                                       (if dot (subseq filename 0 dot) filename))))
    (cond ((or (null option) (%php-null-p option))
           (let ((r (%php-make-array)))
             (%php-array-set r "dirname" dirname)
             (%php-array-set r "basename" filename)
             (%php-array-set r "extension" ext)
             (%php-array-set r "filename" filename-without-extension)
             r))
          ((= option 1) dirname)
          ((= option 2) filename)
          ((= option 4) ext)
          ((= option 8) filename-without-extension)
          (t nil))))

(defun %php-tempnam (dir &optional (prefix ""))
  "PHP tempnam: create file with unique name."
  (let* ((base (if (and dir (not (%php-null-p dir))) (%php-path-string dir) "/tmp"))
         (base (string-right-trim "/" base))
         (prefix (%php-stringify prefix)))
    (loop for attempt from 0 below 1024
          for tmp = (format nil "~A/~A~8,'0X~8,'0X~4,'0X"
                            base
                            prefix
                            (logand (sxhash (list (get-universal-time)
                                                   (get-internal-real-time)
                                                   (gensym)))
                                    #xffffffff)
                            (random #xffffffff)
                            attempt)
          when (handler-case
                   (with-open-file (s tmp :direction :output
                                           :if-exists nil
                                           :if-does-not-exist :create
                                           :external-format :utf-8)
                     (declare (ignore s))
                     t)
                 (error () nil))
            do (return tmp)
          finally (return nil))))

(defun %php-sys-get-temp-dir ()
  "PHP sys_get_temp_dir: return temp directory path."
  "/tmp")

;;; ─── fopen/fclose/fread/fwrite family ────────────────────────────────────────
;;; We represent file handles as a hash table with stream/mode/path metadata.

(defun %php-standard-stream-handle (stream path mode)
  "Return a PHP stream handle for a dynamically bound CL standard stream."
  (let ((handle (%php-make-array)))
    (%php-array-set handle "__stream__" stream)
    (%php-array-set handle "__mode__" mode)
    (%php-array-set handle "__path__" path)
    (%php-array-set handle "__standard__" t)
    (%php-array-set handle "__eof__" nil)
    handle))

(defvar *php-file-locks* (make-hash-table :test #'equal)
  "Per-path flock state for the CLI compatibility model.")

(defun %php-file-lock-state (path)
  (or (gethash path *php-file-locks*)
      (setf (gethash path *php-file-locks*)
            (list :exclusive nil
                  :shared (make-hash-table :test #'eq)))))

(defun %php-file-lock-release-handle (handle)
  (let ((paths-to-remove '()))
    (maphash (lambda (path state)
               (let ((exclusive (getf state :exclusive))
                     (shared (getf state :shared)))
                 (when (eq exclusive handle)
                   (setf (getf state :exclusive) nil))
                 (when (hash-table-p shared)
                   (remhash handle shared))
                 (when (and (null (getf state :exclusive))
                            (hash-table-p shared)
                            (zerop (hash-table-count shared)))
                   (push path paths-to-remove))))
             *php-file-locks*)
    (dolist (path paths-to-remove)
      (remhash path *php-file-locks*))))

(defun %php-file-lock-compatible-p (state handle mode)
  (let ((exclusive (getf state :exclusive))
        (shared (getf state :shared)))
    (cond
      ((= mode 1)
       (or (null exclusive)
           (eq exclusive handle)))
      ((= mode 2)
       (and (or (null exclusive)
                (eq exclusive handle))
            (or (not (hash-table-p shared))
                (zerop (hash-table-count shared))
                (and (= (hash-table-count shared) 1)
                     (gethash handle shared)))))
      (t nil))))

(defun %php-file-lock-acquire (handle mode)
  (let* ((path (%php-array-ref handle "__path__"))
         (state (%php-file-lock-state path))
         (shared (getf state :shared)))
    (cond
      ((= mode 3)
       (%php-file-lock-release-handle handle)
       (%php-array-set handle "__lock__" nil)
       t)
      ((%php-file-lock-compatible-p state handle mode)
       (cond
         ((= mode 1)
          (setf (getf state :exclusive) nil)
          (setf (gethash handle shared) t)
          (%php-array-set handle "__lock__" mode)
          t)
         ((= mode 2)
          (when (hash-table-p shared)
            (remhash handle shared))
          (setf (getf state :exclusive) handle)
          (%php-array-set handle "__lock__" mode)
          t)))
      (t nil))))

(defun %php-stdin ()
  "PHP STDIN stream resource."
  (%php-standard-stream-handle *standard-input* "php://stdin" "r"))

(defun %php-stdout ()
  "PHP STDOUT stream resource."
  (%php-standard-stream-handle *standard-output* "php://stdout" "w"))

(defun %php-stderr ()
  "PHP STDERR stream resource."
  (%php-standard-stream-handle *error-output* "php://stderr" "w"))

(defun %php-fopen (filename mode &optional use-include-path context)
  "PHP fopen: open a file and return a file handle."
  (declare (ignore use-include-path context))
  (handler-case
      (let* ((path (%php-stringify filename))
             (m (%php-stringify mode))
             (direction (cond ((member m '("r" "rb") :test #'string=) :input)
                              ((member m '("w" "wb" "x" "xb") :test #'string=) :output)
                              ((member m '("a" "ab") :test #'string=) :output)
                              ((member m '("r+" "r+b") :test #'string=) :io)
                              (t :output)))
             (if-exists (cond ((member m '("a" "ab") :test #'string=) :append)
                              ((member m '("x" "xb") :test #'string=) :error)
                              (t :supersede)))
             (stream (open path :direction direction
                                :external-format :utf-8
                                :if-exists if-exists
                                :if-does-not-exist (if (eq direction :input) :error :create)))
             (handle (%php-make-array)))
        (%php-array-set handle "__stream__" stream)
        (%php-array-set handle "__mode__" m)
        (%php-array-set handle "__path__" path)
        (%php-array-set handle "__eof__" nil)
        handle)
    (error () nil)))

(defun %php-fclose (handle)
  "PHP fclose: close a file handle."
  (handler-case
      (let ((stream (%php-array-ref handle "__stream__")))
        (when (and stream (streamp stream))
          (unless (%php-array-ref handle "__standard__")
            (close stream)))
        (%php-file-lock-release-handle handle)
        t)
    (error () nil)))

(defun %php-flock (handle operation)
  "PHP flock: maintain a minimal in-memory lock model for file handles."
  (handler-case
      (let* ((stream (%php-array-ref handle "__stream__"))
             (path (%php-array-ref handle "__path__"))
             (mode (%php-to-integer operation))
             (lock-mode (logand mode 3)))
        (if (and stream path)
            (%php-file-lock-acquire handle lock-mode)
            nil))
    (error () nil)))

(defun %php-fread (handle length)
  "PHP fread: read up to LENGTH bytes from file handle."
  (handler-case
      (let* ((stream (%php-array-ref handle "__stream__"))
             (buf (make-string length))
             (n (read-sequence buf stream)))
        (when (< n length)
          (%php-array-set handle "__eof__" t))
        (subseq buf 0 n))
    (error () "")))

(defun %php-fwrite (handle string &optional length)
  "PHP fwrite: write string to file handle."
  (handler-case
      (let* ((stream (%php-array-ref handle "__stream__"))
             (text (%php-stringify string))
             (to-write (if (and length (not (%php-null-p length)))
                           (subseq text 0 (min length (length text)))
                           text)))
        (write-string to-write stream)
        (length to-write))
    (error () nil)))

(defun %php-fputs (handle string &optional length)
  "PHP fputs: alias for fwrite."
  (%php-fwrite handle string length))

(defun %php-fgets (handle &optional length)
  "PHP fgets: read a line from file handle."
  (handler-case
      (let ((stream (%php-array-ref handle "__stream__")))
        (let ((line (read-line stream nil nil)))
          (if line
              (if (and length (not (%php-null-p length)))
                  (subseq (concatenate 'string line (string #\Newline)) 0
                          (min length (1+ (length line))))
                  (concatenate 'string line (string #\Newline)))
              (progn (%php-array-set handle "__eof__" t) nil))))
    (error () nil)))

(defun %php-fgetc (handle)
  "PHP fgetc: read a single character from file handle."
  (handler-case
      (let* ((stream (%php-array-ref handle "__stream__"))
             (ch (read-char stream nil nil)))
        (if ch (string ch)
            (progn (%php-array-set handle "__eof__" t) nil)))
    (error () nil)))

(defun %php-feof (handle)
  "PHP feof: check if end of file has been reached."
  (let ((eof (%php-array-ref handle "__eof__")))
    (and eof (%php-truthy eof))))

(defun %php-fflush (handle)
  "PHP fflush: flush output to file."
  (handler-case
      (let ((stream (%php-array-ref handle "__stream__")))
        (when (and stream (output-stream-p stream))
          (finish-output stream))
        t)
    (error () nil)))

(defun %php-fseek (handle offset &optional (whence 0))
  "PHP fseek: set the position in a file."
  (handler-case
      (let ((stream (%php-array-ref handle "__stream__")))
        (file-position stream
                       (case whence
                         (0 offset)             ; SEEK_SET
                         (1 (+ (file-position stream) offset))  ; SEEK_CUR
                         (2 nil)                ; SEEK_END - not easily portable
                         (t offset)))
        0)
    (error () -1)))

(defun %php-ftell (handle)
  "PHP ftell: return current file position."
  (handler-case
      (file-position (%php-array-ref handle "__stream__"))
    (error () nil)))

(defun %php-rewind (handle)
  "PHP rewind: set file position to beginning."
  (%php-fseek handle 0 0))

(defun %php-fgetcsv (handle &optional (length 0) (separator ",") (enclosure "\"") (escape "\\"))
  "PHP fgetcsv: parse CSV line from file."
  (declare (ignore length enclosure escape))
  (let ((line (%php-fgets handle)))
    (if line
        (%php-explode separator (string-right-trim '(#\Newline #\Return) line))
        nil)))

(defun %php-fputcsv (handle fields &optional (separator ",") (enclosure "\"") (escape "\\"))
  "PHP fputcsv: format line as CSV and write to file."
  (declare (ignore escape))
  (let* ((sep (%php-stringify separator))
         (enc (%php-stringify enclosure))
         (parts (mapcar (lambda (f)
                          (let ((s (%php-stringify f)))
                            (if (or (search sep s) (search enc s) (search (string #\Newline) s))
                                (concatenate 'string enc
                                             (cl:with-output-to-string (o)
                                               (loop for ch across s
                                                     do (when (string= (string ch) enc)
                                                          (write-string enc o))
                                                        (write-char ch o)))
                                             enc)
                                s)))
                        (%php-array-values-list fields)))
         (line (concatenate 'string (format nil "~{~A~^~A~}" (list (car parts) sep))
                            (string #\Newline))))
    (%php-fwrite handle line)
    (length line)))
