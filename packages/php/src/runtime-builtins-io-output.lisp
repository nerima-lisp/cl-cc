;;;; PHP output buffering and response-model builtins.

(in-package :cl-cc/php)

;;; ─── Output buffering and headers ───────────────────────────────────────────

(defvar *php-output-buffer-stack* nil
  "Stack of active PHP output buffers.")

(defvar *php-output-started-p* nil
  "Whether unbuffered PHP output has been emitted.")

(defvar *php-http-response-code* 200
  "Current PHP HTTP response code for CLI response modelling.")

(defvar *php-http-headers* nil
  "Headers queued by PHP header(), newest first.")

(defun %php-make-output-buffer ()
  (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))

(defun %php-output-buffer-string (buffer)
  (coerce buffer 'string))

(defun %php-output-write (value)
  "Write VALUE through PHP output buffering and return nil."
  (let* ((text (%php-stringify value))
         (buffer (first *php-output-buffer-stack*)))
    (if buffer
        (loop for ch across text do (vector-push-extend ch buffer))
        (progn
          (setf *php-output-started-p* t)
          (write-string text)))
    nil))

(defun %php-echo (&rest args)
  "PHP echo builtin: write each argument and return nil."
  (dolist (arg args)
    (%php-output-write arg))
  nil)

(defun %php-print (arg)
  "PHP print builtin: write ARG and return 1."
  (%php-output-write arg)
  1)

(defun %php-ob-start (&optional callback chunk-size flags)
  "PHP ob_start: start output buffering."
  (declare (ignore callback chunk-size flags))
  (push (%php-make-output-buffer) *php-output-buffer-stack*)
  t)

(defun %php-ob-end-clean ()
  "PHP ob_end_clean: discard the current output buffer."
  (if *php-output-buffer-stack*
      (progn
        (pop *php-output-buffer-stack*)
        t)
      nil))

(defun %php-ob-get-clean ()
  "PHP ob_get_clean: get current buffer contents and discard it."
  (if *php-output-buffer-stack*
      (%php-output-buffer-string (pop *php-output-buffer-stack*))
      nil))

(defun %php-ob-get-contents ()
  "PHP ob_get_contents: get current output buffer contents."
  (if *php-output-buffer-stack*
      (%php-output-buffer-string (first *php-output-buffer-stack*))
      nil))

(defun %php-http-response-code (&optional response-code)
  "PHP http_response_code: set/get HTTP response code."
  (if (or (null response-code) (%php-null-p response-code))
      *php-http-response-code*
      (let ((old *php-http-response-code*))
        (setf *php-http-response-code* (%php-to-integer response-code))
        old)))

(defun %php-string-prefix-p (prefix string)
  "Return true when STRING starts with PREFIX, ignoring case."
  (and (>= (length string) (length prefix))
       (string-equal prefix string :end2 (length prefix))))

(defun %php-header-name (line)
  "Return the normalized field name for a header line, or nil."
  (let* ((text (%php-stringify line))
         (pos (position #\: text)))
    (when pos
      (string-downcase (string-trim '(#\Space #\Tab) (subseq text 0 pos))))))

(defun %php-status-line-code (line)
  "Return an HTTP status code encoded in LINE, or nil."
  (let* ((text (%php-stringify line))
         (trimmed (string-trim '(#\Space #\Tab) text)))
    (cond
      ((%php-string-prefix-p "HTTP/" trimmed)
       (let* ((space (position #\Space trimmed))
              (start (and space (1+ space)))
              (end (and start (position #\Space trimmed :start start))))
         (when start
           (parse-integer trimmed :start start :end end :junk-allowed t))))
      ((%php-string-prefix-p "Status:" trimmed)
       (parse-integer trimmed :start (length "Status:") :junk-allowed t))
      (t nil))))

(defun %php-header (header &optional (replace t) response-code)
  "PHP header: queue a response header in the CLI response model."
  (let* ((line (%php-stringify header))
         (code (%php-status-line-code line))
         (name (%php-header-name line)))
    (when (and response-code (not (%php-null-p response-code)))
      (setf *php-http-response-code* (%php-to-integer response-code)))
    (when code
      (setf *php-http-response-code* code))
    (when (and name (%php-truthy replace))
      (setf *php-http-headers*
            (remove name *php-http-headers*
                    :test #'string=
                    :key #'%php-header-name)))
    (push line *php-http-headers*)
    nil))

(defun %php-headers-list ()
  "PHP headers_list: return queued response headers in insertion order."
  (%php-list-to-array (reverse *php-http-headers*)))

(defun %php-headers-sent (&optional file line)
  "PHP headers_sent: report whether unbuffered output has started."
  (declare (ignore file line))
  *php-output-started-p*)

(defun %php-mail
    (to subject message &optional additional-headers additional-params)
  "PHP mail: accept a mail request in the CLI compatibility model."
  (declare (ignore to subject message additional-headers additional-params))
  t)
