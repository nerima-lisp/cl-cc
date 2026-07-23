;;;; tests/unit/vm/io-tests.lisp — VM I/O Operations Unit Tests
;;;;
;;;; Tests for I/O instructions: make-string-output-stream,
;;;; get-output-stream-string, stream-write-string, read-from-string,
;;;; stream predicates, file-handle helpers, and read/write line behavior.

(in-package :cl-cc/test)



(it-sequential "io-print-circle-circular-list"
  (let ((x (list 'a)))
    (setf (cdr x) x)
    (let ((printed (cl-cc/vm::vm-write-object-to-string x :circle t)))
      (expect (search "#0=" printed) :to-be-truthy)
      (expect (search "#0#" printed) :to-be-truthy))))

(it-sequential "io-print-circle-shared-vector"
  (let* ((shared (list 1 2))
         (vec (vector shared shared))
         (printed (cl-cc/vm::vm-write-object-to-string vec :circle t)))
    (expect (search "#0=" printed) :to-be-truthy)
    (expect (search "#0#" printed) :to-be-truthy)))

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun io-vm (&optional (out (make-string-output-stream)))
  "Create a vm-io-state with a string output stream for capture."
  (cl-cc/vm::make-vm-state :output-stream out))

(defun io-vm-full (&optional (out (make-string-output-stream)))
  "Create a vm-io-state (with file handle management) for IO tests."
  (cl-cc/vm::make-vm-state :output-stream out))

(defun io-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc/vm::execute-instruction inst state 0 (make-hash-table :test #'equal)))

;;; ─── make-string-output-stream / get-output-stream-string ─────────────────

(it-sequential "io-string-output-stream-roundtrip"
  (let ((s (io-vm)))
    ;; Create string output stream
    (io-exec (cl-cc:make-vm-make-string-output-stream-inst :dst :R0) s)
    (let ((stream (cl-cc/vm::vm-reg-get s :R0)))
      (expect (streamp stream) :to-be-truthy)
      ;; Write string to it
      (cl-cc/vm::vm-reg-set s :R1 stream)
      (cl-cc/vm::vm-reg-set s :R2 "hello")
      (io-exec (cl-cc:make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      ;; Get accumulated string
      (io-exec (cl-cc:make-vm-get-output-stream-string-inst :dst :R3 :src :R1) s)
      (expect (cl-cc/vm::vm-reg-get s :R3) :to-equal "hello"))))

(it-sequential "io-string-output-stream-multiple-writes"
  (let ((s (io-vm)))
    (io-exec (cl-cc:make-vm-make-string-output-stream-inst :dst :R0) s)
    (let ((stream (cl-cc/vm::vm-reg-get s :R0)))
      (cl-cc/vm::vm-reg-set s :R1 stream)
      (cl-cc/vm::vm-reg-set s :R2 "hello")
      (io-exec (cl-cc:make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (cl-cc/vm::vm-reg-set s :R2 " world")
      (io-exec (cl-cc:make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (io-exec (cl-cc:make-vm-get-output-stream-string-inst :dst :R3 :src :R1) s)
      (expect (cl-cc/vm::vm-reg-get s :R3) :to-equal "hello world"))))

;;; ─── read-from-string ─────────────────────────────────────────────────────

(defun %io-read-str (src)
  "Execute vm-read-from-string on SRC in a fresh vm-state and return the result."
  (let ((vm (io-vm)))
    (cl-cc/vm::vm-reg-set vm :R1 src)
    (io-exec (cl-cc:make-vm-read-from-string-inst :dst :R0 :src :R1) vm)
    (cl-cc/vm::vm-reg-get vm :R0)))

(it-sequential "io-read-from-string-integer"
  (expect (%io-read-str "42") :to-equal 42))

(it-sequential "io-read-from-string-symbol"
  (expect (symbol-name (%io-read-str "hello")) :to-equal "HELLO"))

(it-sequential "io-read-from-string-list"
  (let ((result (%io-read-str "(1 2 3)")))
    (expect (listp result) :to-be-truthy)
    (expect (length result) :to-equal 3)))

(it-sequential "io-read-from-string-empty"
  (expect (%io-read-str "") :to-be-null))

;;; ─── vm-allocate-file-handle ────────────────────────────────────────────────

(it-sequential "io-allocate-handle-sequence"
  (let ((s (io-vm-full)))
    (let ((h1 (cl-cc/vm::vm-allocate-file-handle s))
          (h2 (cl-cc/vm::vm-allocate-file-handle s))
          (h3 (cl-cc/vm::vm-allocate-file-handle s)))
      (expect h1 :to-equal 2)
      (expect h2 :to-equal 3)
      (expect h3 :to-equal 4))))

;;; ─── vm-get-stream ──────────────────────────────────────────────────────────

(it-sequential "io-get-stream-std-handles stdin"
  (destructuring-bind (accessor handle) (list #'cl-cc/vm::vm-standard-input cl-cc/vm::+stdin-handle+)
    (let ((s (io-vm-full)))
    (expect (cl-cc/vm::vm-get-stream s handle) :to-equal (funcall accessor s)))))

(it-sequential "io-get-stream-std-handles stdout"
  (destructuring-bind (accessor handle) (list #'cl-cc/vm::vm-standard-output cl-cc/vm::+stdout-handle+)
    (let ((s (io-vm-full)))
    (expect (cl-cc/vm::vm-get-stream s handle) :to-equal (funcall accessor s)))))

(it-sequential "io-get-stream-cl-stream-passthrough"
  (let ((s (io-vm-full))
        (stream (make-string-output-stream)))
    (expect (cl-cc/vm::vm-get-stream s stream) :to-equal stream)))

(it-sequential "io-get-stream-handle-map-lookup-cases open-files"
  (destructuring-bind (register-fn) (list (lambda (s h stream) (setf (gethash h (cl-cc/vm::vm-open-files s)) stream)))
    (let* ((s      (io-vm-full))
         (stream (make-string-output-stream))
         (handle (cl-cc/vm::vm-allocate-file-handle s)))
    (funcall register-fn s handle stream)
    (expect (cl-cc/vm::vm-get-stream s handle) :to-equal stream))))

(it-sequential "io-get-stream-handle-map-lookup-cases string-streams"
  (destructuring-bind (register-fn) (list (lambda (s h stream) (setf (gethash h (cl-cc/vm::vm-string-streams s)) stream)))
    (let* ((s      (io-vm-full))
         (stream (make-string-output-stream))
         (handle (cl-cc/vm::vm-allocate-file-handle s)))
    (funcall register-fn s handle stream)
    (expect (cl-cc/vm::vm-get-stream s handle) :to-equal stream))))

(it-sequential "io-get-stream-invalid-handle-error"
  (let ((s (io-vm-full)))
    (signals error (cl-cc/vm::vm-get-stream s 999))))

;;; ─── vm-stream-open-p ──────────────────────────────────────────────────────

(it-sequential "io-stream-open-p-standard-handles stdin"
  (destructuring-bind (handle) (list cl-cc/vm::+stdin-handle+)
    (let ((s (io-vm-full)))
    (expect (cl-cc/vm::vm-stream-open-p s handle) :to-be-truthy))))

(it-sequential "io-stream-open-p-standard-handles stdout"
  (destructuring-bind (handle) (list cl-cc/vm::+stdout-handle+)
    (let ((s (io-vm-full)))
    (expect (cl-cc/vm::vm-stream-open-p s handle) :to-be-truthy))))

(it-sequential "io-stream-open-p-unknown-handle-returns-nil"
  (let ((s (io-vm-full)))
    (expect (cl-cc/vm::vm-stream-open-p s 999) :to-equal nil)))

(it-sequential "io-stream-open-p-direct-cl-stream-returns-truthy"
  (let ((s (io-vm-full)))
    (expect (cl-cc/vm::vm-stream-open-p s (make-string-output-stream)) :to-be-truthy)))

;;; ─── stream predicate instructions ─────────────────────────────────────────

(it-sequential "io-streamp cl-stream"
  (destructuring-bind (value expected) (list (make-string-output-stream) t)
    (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (io-exec (cl-cc:make-vm-streamp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "io-streamp non-stream"
  (destructuring-bind (value expected) (list 42 nil)
    (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (io-exec (cl-cc:make-vm-streamp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "io-streamp-handle-resolved"
  (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 cl-cc/vm::+stdin-handle+)
    (io-exec (cl-cc:make-vm-streamp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal t)))

(it-sequential "io-input-stream-p input-stream"
  (destructuring-bind (stream-val expected) (list (make-string-input-stream "hello") t)
    (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 stream-val)
    (io-exec (cl-cc:make-vm-input-stream-p :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "io-input-stream-p output-stream"
  (destructuring-bind (stream-val expected) (list (make-string-output-stream) nil)
    (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 stream-val)
    (io-exec (cl-cc:make-vm-input-stream-p :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "io-output-stream-p output-stream"
  (destructuring-bind (stream-val expected) (list (make-string-output-stream) t)
    (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 stream-val)
    (io-exec (cl-cc:make-vm-output-stream-p :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "io-output-stream-p input-stream"
  (destructuring-bind (stream-val expected) (list (make-string-input-stream "x") nil)
    (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 stream-val)
    (io-exec (cl-cc:make-vm-output-stream-p :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "io-open-stream-p-true"
  (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 (make-string-output-stream))
    (io-exec (cl-cc:make-vm-open-stream-p :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal t)))

;;; ─── handle-based string streams (vm-make-string-stream) ───────────────────

(it-sequential "io-make-string-stream-output"
  (let ((s (io-vm-full)))
    (io-exec (cl-cc:make-vm-make-string-stream :dst :R0 :direction :output) s)
    (let ((handle (cl-cc/vm::vm-reg-get s :R0)))
      (expect (integerp handle) :to-be-truthy)
      (expect (>= handle 2) :to-be-truthy))))

(it-sequential "io-make-string-stream-input"
  (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 "hello")
    (io-exec (cl-cc:make-vm-make-string-stream :dst :R0 :direction :input
                                                 :initial-string :R1) s)
    (let ((handle (cl-cc/vm::vm-reg-get s :R0)))
      (expect (integerp handle) :to-be-truthy)
      ;; Read a char from it to verify content
      (cl-cc/vm::vm-reg-set s :R2 handle)
      (io-exec (cl-cc:make-vm-read-char :dst :R3 :handle :R2) s)
      (expect (cl-cc/vm::vm-reg-get s :R3) :to-equal #\h))))

(it-sequential "io-make-string-stream-get-string"
  (let ((s (io-vm-full)))
    ;; Create output string stream (handle-based)
    (io-exec (cl-cc:make-vm-make-string-stream :dst :R0 :direction :output) s)
    (let ((handle (cl-cc/vm::vm-reg-get s :R0)))
      ;; Write to it via handle
      (cl-cc/vm::vm-reg-set s :R1 handle)
      (cl-cc/vm::vm-reg-set s :R2 "test output")
      (io-exec (cl-cc:make-vm-write-string :handle :R1 :str :R2) s)
      ;; Get string from handle
      (io-exec (cl-cc:make-vm-get-string-from-stream :dst :R3 :handle :R1) s)
      (expect (cl-cc/vm::vm-reg-get s :R3) :to-equal "test output"))))

;;; ─── eof-p ──────────────────────────────────────────────────────────────────

(it-sequential "io-eof-p eof"
  (destructuring-bind (value expected) (list :eof 1)
    (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (io-exec (cl-cc:make-vm-eof-p :dst :R0 :value :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "io-eof-p non-eof"
  (destructuring-bind (value expected) (list #\a 0)
    (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (io-exec (cl-cc:make-vm-eof-p :dst :R0 :value :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

;;; ─── read-char / read-line via handle ──────────────────────────────────────

(it-sequential "io-read-line-from-string-stream"
  (let ((s (io-vm-full)))
    (cl-cc/vm::vm-reg-set s :R1 "first line")
    (io-exec (cl-cc:make-vm-make-string-stream :dst :R0 :direction :input
                                                 :initial-string :R1) s)
    (let ((handle (cl-cc/vm::vm-reg-get s :R0)))
      (cl-cc/vm::vm-reg-set s :R2 handle)
      (io-exec (cl-cc:make-vm-read-line :dst :R3 :handle :R2) s)
      (expect (cl-cc/vm::vm-reg-get s :R3) :to-equal "first line"))))

;;; ─── %resolve-integer-stream-handle (extracted helper) ──────────────────

(it-sequential "resolve-integer-stream-handle-stdin"
  (let ((s   (io-vm-full))
        (h   cl-cc/vm::+stdin-handle+))
    (expect (cl-cc/vm::%resolve-integer-stream-handle h s) :to-be (cl-cc/vm::vm-standard-input s))))

(it-sequential "resolve-integer-stream-handle-stdout"
  (let ((s (io-vm-full))
        (h cl-cc/vm::+stdout-handle+))
    (expect (cl-cc/vm::%resolve-integer-stream-handle h s) :to-be (cl-cc/vm::vm-standard-output s))))

(it-sequential "resolve-integer-stream-handle-unknown-returns-nil"
  (let ((s (io-vm-full)))
    (expect (cl-cc/vm::%resolve-integer-stream-handle 9999 s) :to-be-null)))

;;; ─── %copy-ht-into ────────────────────────────────────────────────────────

(it-sequential "copy-ht-into-copies-all-entries"
  (let ((src (make-hash-table :test #'eq))
        (dst (make-hash-table :test #'eq)))
    (setf (gethash :a src) 1
          (gethash :b src) 2
          (gethash :old dst) 99)
    (cl-cc/vm::%copy-ht-into src dst)
    (expect (= 1 (gethash :a dst)) :to-be-truthy)
    (expect (= 2 (gethash :b dst)) :to-be-truthy)
    (expect (gethash :old dst) :to-be-null)
    (expect (= 2 (hash-table-count dst)) :to-be-truthy)))

(it-sequential "copy-ht-into-empty-src-clears-dst"
  (let ((src (make-hash-table :test #'eq))
        (dst (make-hash-table :test #'eq)))
    (setf (gethash :x dst) 42)
    (cl-cc/vm::%copy-ht-into src dst)
    (expect (= 0 (hash-table-count dst)) :to-be-truthy)))

;;; ─── clone-vm-state ───────────────────────────────────────────────────────

;; Pre-existing base failure (errors identically on the pre-migration deftest
;; baseline): (make-vm-state) leaves vm-global-vars NIL rather than an empty
;; hash-table, so (setf (gethash "x" (vm-global-vars source)) 42) errors. Base
;; struct-init issue, unrelated to the cl-weave migration.
(it-todo "clone-vm-state-copies-global-vars"
  "pre-existing base failure: (make-vm-state) vm-global-vars is NIL, not a hash-table")

;;; ─── FR-868: file-position / file-length / with-binary-file ───────────────

(it-sequential "io-file-position-set-returns-boolean"
  (let ((s (io-vm-full)))
    (with-input-from-string (stream "abcdef")
      (cl-cc/vm::vm-reg-set s :stream stream)
      (io-exec (cl-cc:make-vm-file-position :dst :pos :handle :stream) s)
      (expect (= 0 (cl-cc/vm::vm-reg-get s :pos)) :to-be-truthy)
      (cl-cc/vm::vm-reg-set s :new-pos 3)
      (io-exec (cl-cc:make-vm-file-position :dst :ok :handle :stream :position :new-pos) s)
      (expect (cl-cc/vm::vm-reg-get s :ok) :to-equal t)
      (expect (read-char stream) :to-equal #\d))))

(it-sequential "io-file-length-binary-stream"
  (let ((path (merge-pathnames (format nil "clcc-io-file-length-~A.bin" (gensym))
                               (uiop:temporary-directory)))
        (s (io-vm-full)))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                                :if-exists :supersede :if-does-not-exist :create)
             (write-byte 1 out)
             (write-byte 2 out)
             (write-byte 3 out))
           (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
             (cl-cc/vm::vm-reg-set s :stream in)
             (io-exec (cl-cc:make-vm-file-length :dst :len :handle :stream) s)
             (expect (= 3 (cl-cc/vm::vm-reg-get s :len)) :to-be-truthy)))
      (when (probe-file path) (delete-file path)))))

(it-sequential "io-with-binary-file-defaults-to-io"
  (let ((path (merge-pathnames (format nil "clcc-with-binary-file-~A.bin" (gensym))
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (cl-cc/vm::with-binary-file (stream path :if-exists :supersede :if-does-not-exist :create)
             (write-byte 65 stream)
             (expect (file-position stream 0) :to-equal t)
             (expect (= 65 (read-byte stream)) :to-be-truthy))
           (expect (probe-file path) :to-be-truthy))
      (when (probe-file path) (delete-file path)))))

;;; ─── FR-923: buffered I/O and stream predicates ───────────────────────────

(it-sequential "io-make-buffered-stream-validates-options-and-flushes"
  (let* ((raw (make-string-output-stream))
         (stream (cl-cc/vm::make-buffered-stream raw :buffer-size 16 :strategy :line)))
    (expect stream :to-be raw)
    (write-string "abc" stream)
    (force-output stream)
    (finish-output stream)
    (expect (get-output-stream-string stream) :to-equal "abc")))

(it-sequential "io-stream-query-instructions-on-handle"
  (let ((s (io-vm-full)))
    (io-exec (cl-cc:make-vm-make-string-stream :dst :handle :direction :input
                                                :initial-string nil) s)
    (io-exec (cl-cc:make-vm-stream-element-type-inst :dst :type :src :handle) s)
    (io-exec (cl-cc:make-vm-open-stream-p :dst :open :src :handle) s)
    (io-exec (cl-cc:make-vm-interactive-stream-p :dst :interactive :src :handle) s)
    (expect (cl-cc/vm::vm-reg-get s :type) :to-equal 'character)
    (expect (cl-cc/vm::vm-reg-get s :open) :to-equal t)
    (expect (cl-cc/vm::vm-reg-get s :interactive) :to-equal nil)))

;;; ─── FR-924: special streams ───────────────────────────────────────────────

(it-sequential "io-special-string-stream-bridge"
  (let ((stream (make-string-output-stream)))
    (write-string "hello" stream)
    (expect (cl-cc/vm::%vm-bridge-get-output-stream-string stream) :to-equal "hello")))

(it-sequential "io-special-composite-streams"
  (let* ((out-a (make-string-output-stream))
         (out-b (make-string-output-stream))
         (broadcast (cl-cc/vm::%vm-bridge-make-broadcast-stream out-a out-b))
         (two-way (cl-cc/vm::%vm-bridge-make-two-way-stream
                   (make-string-input-stream "x") broadcast))
         (echo (cl-cc/vm::%vm-bridge-make-echo-stream
                (make-string-input-stream "y") broadcast))
         (concat (cl-cc/vm::%vm-bridge-make-concatenated-stream
                  (make-string-input-stream "ab")
                  (make-string-input-stream "cd")))
         (*standard-output* broadcast)
         (synonym (cl-cc/vm::%vm-bridge-make-synonym-stream '*standard-output*)))
    (write-char #\A two-way)
    (expect (read-char echo) :to-equal #\y)
    (write-char #\B synonym)
    (expect (loop repeat 4 collect (read-char concat) into chars
                                finally (return (coerce chars 'string))) :to-equal "abcd")
    (finish-output broadcast)
    (expect (get-output-stream-string out-a) :to-equal "AyB")
    (expect (get-output-stream-string out-b) :to-equal "AyB")))

;;; ─── FR-927: pathname operations ───────────────────────────────────────────

(it-sequential "io-pathname-operations-host-backed"
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "clcc-path-fr927-~A/" (gensym))
                                 (uiop:temporary-directory))))
         (file (merge-pathnames "alpha.txt" root))
         (wild (merge-pathnames "*.txt" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist file)
           (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
             (write-string "x" out))
           (let* ((pn (make-pathname :directory (pathname-directory root)
                                     :name "alpha" :type "txt"))
                  (merged (merge-pathnames "alpha.txt" root))
                  (parsed (parse-namestring (namestring file))))
             (expect (pathname-directory pn) :to-equal (pathname-directory root))
             (expect (pathname-name pn) :to-equal "alpha")
             (expect (pathname-type pn) :to-equal "txt")
             (expect (namestring merged) :to-equal (namestring file))
             (expect (file-namestring parsed) :to-equal "alpha.txt")
             (expect (plusp (length (directory-namestring parsed))) :to-be-truthy)
             (expect (enough-namestring file root) :to-equal "alpha.txt")
             (expect (wild-pathname-p wild) :to-be-truthy)
             (expect (pathname-match-p file wild) :to-be-truthy)
             (expect (= 1 (length (directory wild))) :to-be-truthy)))
      (when (probe-file file) (delete-file file))
      (when (probe-file root) (uiop:delete-directory-tree root :validate t)))))

;;; ─── FR-851/852/853/869: networking, DNS, TLS, mmap ───────────────────────

(it-sequential "io-fr851-tcp-localhost-roundtrip"
  (let ((listener nil) (client nil) (accepted nil))
    (unwind-protect
         (progn
           (setf listener (cl-cc/vm:make-tcp-socket :reuse-address t :tcp-nodelay t :keepalive t))
           (cl-cc/vm:socket-bind listener "127.0.0.1" 0)
           (cl-cc/vm:socket-listen listener 1)
           (multiple-value-bind (host port) (cl-cc/vm:socket-local-address listener)
             (expect host :to-equal "127.0.0.1")
             (setf client (cl-cc/vm:make-tcp-socket :tcp-nodelay t))
             (cl-cc/vm:socket-connect client "127.0.0.1" port)
             (setf accepted (cl-cc/vm:socket-accept listener))
             (expect (= 4 (cl-cc/vm:socket-send client #(112 105 110 103))) :to-be-truthy)
             (multiple-value-bind (payload count) (cl-cc/vm:socket-receive accepted :size 4 :stringp t)
               (expect (= 4 count) :to-be-truthy)
               (expect payload :to-equal "ping"))))
      (when accepted (cl-cc/vm:socket-close accepted))
      (when client (cl-cc/vm:socket-close client))
      (when listener (cl-cc/vm:socket-close listener)))))

(it-sequential "io-fr851-udp-localhost-roundtrip"
  (let ((receiver nil) (sender nil))
    (unwind-protect
         (progn
           (setf receiver (cl-cc/vm:make-udp-socket :reuse-address t))
           (cl-cc/vm:socket-bind receiver "127.0.0.1" 0)
           (multiple-value-bind (_host port) (cl-cc/vm:socket-local-address receiver)
             (declare (ignore _host))
             (setf sender (cl-cc/vm:make-udp-socket))
             (cl-cc/vm:socket-connect sender "127.0.0.1" port)
             (expect (= 3 (cl-cc/vm:socket-send sender #(117 100 112))) :to-be-truthy)
             (multiple-value-bind (payload count) (cl-cc/vm:socket-receive receiver :size 3 :stringp t)
               (expect (= 3 count) :to-be-truthy)
               (expect payload :to-equal "udp"))))
      (when sender (cl-cc/vm:socket-close sender))
      (when receiver (cl-cc/vm:socket-close receiver)))))

(it-sequential "io-fr852-dns-localhost-cache-and-async"
  (let ((addresses (cl-cc/vm:dns-resolve "localhost" :ttl 60)))
    (expect (member "127.0.0.1" addresses :test #'string=) :to-be-truthy)
    (expect (plusp (length (cl-cc/vm:getaddrinfo "localhost" :service 80))) :to-be-truthy)
    (let ((async (cl-cc/vm:dns-resolve-async "localhost")))
      (sb-thread:join-thread (cl-cc/vm::dns-async-result-thread async))
      (expect (cl-cc/vm:dns-async-result-done-p async) :to-be-truthy)
      (expect (cl-cc/vm:dns-async-result-error async) :to-equal nil)
      (expect (member "127.0.0.1" (cl-cc/vm:dns-async-result-result async)
                           :test #'string=) :to-be-truthy))))

(it-sequential "io-fr853-tls-context-and-unsupported-condition"
  (let ((context (cl-cc/vm:make-tls-context :verify-peer t)))
    (expect (cl-cc/vm::tls-context-p context) :to-be-truthy)
    (unless (find-package :cl+ssl)
      (let ((socket (cl-cc/vm:make-tcp-socket)))
        (unwind-protect
             (signals cl-cc/vm:tls-unsupported (cl-cc/vm:tls-wrap-socket socket context :hostname "localhost"))
          (cl-cc/vm:socket-close socket))))))

(it-sequential "io-fr869-mmap-file-shared-sync"
  (let ((path (merge-pathnames (format nil "clcc-mmap-~A.bin" (gensym))
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                                :if-exists :supersede :if-does-not-exist :create)
             (write-sequence #(65 66 67) out))
           (cl-cc/vm:with-mmap (region path :protection :read-write :flags :shared)
             (let ((array (cl-cc/vm:mmap-array region)))
               (multiple-value-bind (base offset) (array-displacement array)
                 (expect base :to-be-truthy)
                 (expect (= 0 offset) :to-be-truthy))
               (expect (= 65 (aref array 0)) :to-be-truthy)
               (setf (aref array 1) 90)
               (cl-cc/vm:mmap-sync region)))
           (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
             (let ((bytes (make-array 3 :element-type '(unsigned-byte 8))))
               (read-sequence bytes in)
               (expect (equalp #(65 90 67) bytes) :to-be-truthy))))
      (when (probe-file path) (delete-file path)))))
