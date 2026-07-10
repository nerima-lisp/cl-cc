;;;; packages/javascript/src/runtime-builtins-platform-url.lisp — URL / URLSearchParams helpers

(in-package :cl-cc/javascript)

(defun %js-split-string-on-char (string char)
  (let ((start 0)
        (parts nil))
    (loop for pos = (position char string :start start)
          do (push (subseq string start pos) parts)
          if pos
            do (setf start (1+ pos))
          else
            return (nreverse parts))))

(defun %js-url-decode-component (string)
  (with-output-to-string (out)
    (loop for i from 0 below (length string)
          for ch = (char string i)
          do (cond
               ((char= ch #\+)
                (write-char #\Space out))
               ((and (char= ch #\%)
                     (< (+ i 2) (length string))
                     (digit-char-p (char string (1+ i)) 16)
                     (digit-char-p (char string (+ i 2)) 16))
                (write-char
                 (code-char (+ (* 16 (digit-char-p (char string (1+ i)) 16))
                               (digit-char-p (char string (+ i 2)) 16)))
                 out)
                (incf i 2))
               (t
                (write-char ch out))))))

(defun %js-url-encode-component (value)
  (let ((string (%js-to-string value)))
    (with-output-to-string (out)
      (loop for ch across string
            for code = (char-code ch)
            do (cond
                 ((or (and (>= code (char-code #\a)) (<= code (char-code #\z)))
                      (and (>= code (char-code #\A)) (<= code (char-code #\Z)))
                      (and (>= code (char-code #\0)) (<= code (char-code #\9)))
                      (member ch '(#\- #\_ #\. #\*) :test #'char=))
                  (write-char ch out))
                 ((char= ch #\Space)
                  (write-char #\+ out))
                 (t
                  (format out "%~2,'0X" code)))))))

(defun %js-url-search-params-pairs (init)
  (cond
    ((or (null init) (eq init +js-undefined+))
     nil)
    ((stringp init)
     (let ((query (if (and (plusp (length init)) (char= (char init 0) #\?))
                      (subseq init 1)
                      init)))
       (loop for part in (%js-split-string-on-char query #\&)
             unless (string= part "")
               collect (let* ((eq-pos (position #\= part))
                              (key (if eq-pos (subseq part 0 eq-pos) part))
                              (value (if eq-pos (subseq part (1+ eq-pos)) "")))
                         (cons (%js-url-decode-component key)
                               (%js-url-decode-component value))))))
    ((%js-ht-p init)
     (let ((pairs nil))
       (maphash (lambda (key value)
                  (push (cons (%js-to-string key) (%js-to-string value)) pairs))
                init)
       (nreverse pairs)))
    ((%js-vec-p init)
     (loop for pair across init
           when (and (%js-vec-p pair) (>= (length pair) 2))
             collect (cons (%js-to-string (aref pair 0))
                           (%js-to-string (aref pair 1)))))
    (t nil)))

(defun %js-url-search-params-string (pairs)
  (format nil "~{~A~^&~}"
          (mapcar (lambda (pair)
                    (format nil "~A=~A"
                            (%js-url-encode-component (car pair))
                            (%js-url-encode-component (cdr pair))))
                  pairs)))

(defun %js-url-normalize-path (path)
  "Normalize URL path dot segments while preserving absolute/trailing slashes."
  (let ((absolute-p (and (plusp (length path)) (char= (char path 0) #\/)))
        (trailing-slash-p (and (> (length path) 1)
                               (char= (char path (1- (length path))) #\/)))
        (segments nil))
    (dolist (segment (%js-split-string-on-char path #\/))
      (cond
        ((or (string= segment "") (string= segment "."))
         nil)
        ((string= segment "..")
         (cond
           (segments (pop segments))
           ((not absolute-p) (push segment segments))))
        (t
         (push segment segments))))
    (let* ((body (format nil "~{~A~^/~}" (nreverse segments)))
           (with-leading (if absolute-p
                             (concatenate 'string "/" body)
                             body)))
      (cond
        ((and absolute-p (string= body "")) "/")
        ((and trailing-slash-p (not (string= with-leading "")))
         (concatenate 'string with-leading "/"))
        (t with-leading)))))

(defun %js-make-url-search-params (&optional init on-change)
  "URLSearchParams: ordered key/value query storage with common mutators."
  (let ((pairs (%js-url-search-params-pairs init)))
    (labels ((changed ()
               (when on-change
                 (funcall on-change (%js-url-search-params-string pairs)))
               +js-undefined+)
             (key= (pair key)
               (string= (car pair) (%js-to-string key)))
             (values-for (key)
               (loop for pair in pairs
                     when (key= pair key)
                       collect (cdr pair)))
             (serialize ()
               (%js-url-search-params-string pairs)))
      (%js-make-object
       "append" (lambda (key value)
                  (setf pairs (append pairs (list (cons (%js-to-string key)
                                                        (%js-to-string value)))))
                  (changed))
       "delete" (lambda (key)
                  (setf pairs (remove-if (lambda (pair) (key= pair key)) pairs))
                  (changed))
       "get" (lambda (key)
               (let ((values (values-for key)))
                 (if values (first values) +js-null+)))
       "getAll" (lambda (key)
                  (apply #'%js-make-array (values-for key)))
       "has" (lambda (key)
               (not (null (values-for key))))
       "set" (lambda (key value)
               (let ((string-key (%js-to-string key))
                     (string-value (%js-to-string value))
                     (written nil)
                     (result nil))
                 (dolist (pair pairs)
                   (cond
                     ((string= (car pair) string-key)
                      (unless written
                        (push (cons string-key string-value) result)
                        (setf written t)))
                     (t
                      (push pair result))))
                 (unless written
                   (push (cons string-key string-value) result))
                 (setf pairs (nreverse result))
                 (changed)))
       "sort" (lambda ()
                (setf pairs
                      (stable-sort pairs
                                   (lambda (a b)
                                     (string< (car a) (car b)))))
                (changed))
       "toString" #'serialize))))

(defun %js-url-build-href (protocol host pathname search hash)
  (concatenate 'string
               protocol
               (if (and (plusp (length protocol)) (plusp (length host))) "//" "")
               host
               pathname
               search
               hash))

(defun %js-parse-url-record (url &optional base)
  (let* ((input (%js-to-string url))
         (base-record (and base
                           (not (or (null base) (eq base +js-undefined+)))
                           (%js-parse-url-record base)))
         (absolute-p (search "://" input))
         (resolved (cond
                     (absolute-p input)
                     (base-record
                      (let* ((base-origin (getf base-record :origin))
                             (base-path (getf base-record :pathname))
                             (base-search (getf base-record :search))
                             (slash (position #\/ base-path :from-end t))
                             (base-dir (if slash (subseq base-path 0 (1+ slash)) "/")))
                        (cond
                          ((and (plusp (length input)) (char= (char input 0) #\/))
                           (concatenate 'string base-origin input))
                          ((and (plusp (length input)) (char= (char input 0) #\?))
                           (concatenate 'string base-origin base-path input))
                          ((and (plusp (length input)) (char= (char input 0) #\#))
                           (concatenate 'string base-origin base-path base-search input))
                          (t
                           (concatenate 'string base-origin base-dir input)))))
                     (t input)))
         (hash-pos (position #\# resolved))
         (before-hash (if hash-pos (subseq resolved 0 hash-pos) resolved))
         (hash (if hash-pos (subseq resolved hash-pos) ""))
         (search-pos (position #\? before-hash))
         (before-search (if search-pos (subseq before-hash 0 search-pos) before-hash))
         (search (if search-pos (subseq before-hash search-pos) ""))
         (scheme-pos (search "://" before-search))
         (protocol (if scheme-pos (subseq before-search 0 (1+ scheme-pos)) ""))
         (after-scheme (if scheme-pos (subseq before-search (+ scheme-pos 3)) before-search))
         (path-pos (position #\/ after-scheme))
         (host (if scheme-pos
                   (if path-pos (subseq after-scheme 0 path-pos) after-scheme)
                   ""))
         (pathname (cond
                     (path-pos (subseq after-scheme path-pos))
                     (scheme-pos "/")
                     ((and (plusp (length before-search))
                           (char= (char before-search 0) #\/))
                      before-search)
                     (t before-search)))
         (normalized-pathname (%js-url-normalize-path pathname))
         (origin (if (and (plusp (length protocol)) (plusp (length host)))
                     (concatenate 'string protocol "//" host)
                     "")))
    (list :protocol protocol
          :host host
          :hostname (let ((colon (position #\: host :from-end t)))
                      (if colon (subseq host 0 colon) host))
          :port (let ((colon (position #\: host :from-end t)))
                  (if colon (subseq host (1+ colon)) ""))
          :origin origin
          :pathname (if (and scheme-pos (string= normalized-pathname ""))
                        "/"
                        normalized-pathname)
          :search search
          :hash hash)))

(defun %js-make-url (url &optional base)
  "URL constructor: parse common absolute and base-relative URL fields."
  (let* ((record (%js-parse-url-record url base))
         (protocol (getf record :protocol))
         (host (getf record :host))
         (origin (getf record :origin))
         (pathname (getf record :pathname))
         (search (getf record :search))
         (hash (getf record :hash))
         (href (%js-url-build-href protocol host pathname search hash))
         object)
    (setf object
          (%js-make-object
           "href" href
           "protocol" protocol
           "host" host
           "hostname" (getf record :hostname)
           "port" (getf record :port)
           "origin" origin
           "pathname" pathname
           "search" search
           "hash" hash
           "toString" (lambda () (gethash "href" object))))
    (setf (gethash "searchParams" object)
          (%js-make-url-search-params
           search
           (lambda (query)
             (let ((next-search (if (string= query "")
                                    ""
                                    (concatenate 'string "?" query))))
               (setf (gethash "search" object) next-search
                     (gethash "href" object)
                     (%js-url-build-href
                      (gethash "protocol" object)
                      (gethash "host" object)
                      (gethash "pathname" object)
                      next-search
                      (gethash "hash" object)))))))
    object))
