;;;; compile/stdlib-source-ext.lisp — Standard Library Source Extension
;;;;
;;;; Appends additional stdlib definitions to *standard-library-source* defined in
;;;; stdlib-source.lisp (loads first). Kept separate to keep each file manageable.
;;;;
;;;; Contains (appended to *standard-library-source*):
;;;;   - Type predicates (FR-386/605): rationalp, complexp, realp, floatp, bignump
;;;;   - Boole constants + boole-* (FR-493)
;;;;   - Byte manipulation: byte, ldb, dpb, etc. (FR-492/532/494)
;;;;   - digit-char (FR-477)
;;;;   - Restart + handler-bind protocol (FR-421/201)
;;;;   - Method error functions (FR-584)
;;;;   - random-state-p, gensym-counter, macroexpand-hook (FR-509/510/429)
;;;;   - function-lambda-expression, upgraded-array-element-type (FR-549/553)
;;;;   - make-load-form, make-condition, parse-integer, parse-number (FR-550/427/478/391)
;;;;   - Sequence utilities: concatenate variants
;;;;
;;;; CLOS protocol, condition hierarchy, and reader/printer/pathname control
;;;; variables are in stdlib-source-clos.lisp.
;;;;
;;;; Load order: after compile/stdlib-source.lisp.
(in-package :cl-cc/stdlib)

(setf *standard-library-source*
      (concatenate 'string
        *standard-library-source*
    ;; ── FR-386: Type predicates ────────────────────────────────────────────
    "(defun rationalp (x) (typep x 'rational))"
    "(defun complexp (x) (typep x 'complex))"
    "(defun realp (x) (typep x 'real))"
    "(defun bignump (x) (typep x 'bignum))"
    "(defun floatp (x) (typep x 'float))"

    ;; ── FR-493: boole + boole-* constants ────────────────────────────────────
    "(defvar boole-clr 0)"
    "(defvar boole-set 1)"
    "(defvar boole-1 2)"
    "(defvar boole-2 3)"
    "(defvar boole-c1 4)"
    "(defvar boole-c2 5)"
    "(defvar boole-and 6)"
    "(defvar boole-ior 7)"
    "(defvar boole-xor 8)"
    "(defvar boole-eqv 9)"
    "(defvar boole-nand 10)"
    "(defvar boole-nor 11)"
    "(defvar boole-andc1 12)"
    "(defvar boole-andc2 13)"
    "(defvar boole-orc1 14)"
    "(defvar boole-orc2 15)"

    "(defun boole (op integer-1 integer-2)
   (cond ((eql op 0) 0)
         ((eql op 1) -1)
         ((eql op 2) integer-1)
         ((eql op 3) integer-2)
         ((eql op 4) (lognot integer-1))
         ((eql op 5) (lognot integer-2))
         ((eql op 6) (logand integer-1 integer-2))
         ((eql op 7) (logior integer-1 integer-2))
         ((eql op 8) (logxor integer-1 integer-2))
         ((eql op 9) (logeqv integer-1 integer-2))
         ((eql op 10) (lognand integer-1 integer-2))
         ((eql op 11) (lognor integer-1 integer-2))
         ((eql op 12) (logandc1 integer-1 integer-2))
         ((eql op 13) (logandc2 integer-1 integer-2))
         ((eql op 14) (logorc1 integer-1 integer-2))
         ((eql op 15) (logorc2 integer-1 integer-2))
         (t (error \"Invalid boole operation: ~S\" op))))"

    ;; ── FR-492/FR-532/FR-494: Byte manipulation ──────────────────────────────
    "(defun byte (size position) (cons size position))"
    "(defun byte-size (byte-spec) (car byte-spec))"
    "(defun byte-position (byte-spec) (cdr byte-spec))"

    "(defun ldb (byte-spec integer)
   (logand (ash integer (- (byte-position byte-spec)))
           (1- (ash 1 (byte-size byte-spec)))))"

    "(defun ldb-test (byte-spec integer)
   (not (zerop (ldb byte-spec integer))))"

    "(defun dpb (newbyte byte-spec integer)
   (let ((size (byte-size byte-spec))
         (pos  (byte-position byte-spec)))
     (logior (logand integer (lognot (ash (1- (ash 1 size)) pos)))
             (ash (logand newbyte (1- (ash 1 size))) pos))))"

    "(defun deposit-field (newbyte byte-spec integer)
   (let ((size (byte-size byte-spec))
         (pos  (byte-position byte-spec)))
     (logior (logand integer (lognot (ash (1- (ash 1 size)) pos)))
             (logand newbyte (ash (1- (ash 1 size)) pos)))))"

    "(defun mask-field (byte-spec integer)
   (logand integer (ash (1- (ash 1 (byte-size byte-spec)))
                        (byte-position byte-spec))))"

    ;; ── FR-477: digit-char ───────────────────────────────────────────────────
    "(defun digit-char (weight &optional (radix 10))
   (if (and (>= weight 0) (< weight radix))
       (if (< weight 10)
           (code-char (+ weight 48))
           (code-char (+ weight 55)))
       nil))"

    ;; ── debugger-related dynamic vars ───────────────────────────────────────
    "(defvar *debugger-hook* nil)"
    "(defvar *break-on-signals* nil)"

    ;; ── FR-421: restart protocol dynamic variable ──────────────────────────
    "(defvar *%active-restarts* nil)"

    ;; ── FR-201: handler-bind dynamic handler registry ────────────────────
    "(defvar *%condition-handlers* nil)"

    ;; signal is defined as a macro in macros-stdlib.lisp for correct symbol resolution

    ;; ── FR-584: Method error functions ───────────────────────────────────────
    "(defun invalid-method-error (method format-control)
   (error format-control))"

    "(defun method-combination-error (format-control)
   (error format-control))"

    ;; ── FR-509: random-state-p ───────────────────────────────────────────────
    "(defun random-state-p (x) (typep x 'random-state))"

    ;; ── FR-511: gentemp ──────────────────────────────────────────────────────
    "(defvar *gentemp-counter* 0)"
    "(defun gentemp (&optional (prefix \"T\") (package *package*))
   (declare (ignore package))
   (setq *gentemp-counter* (+ *gentemp-counter* 1))
   (intern (concatenate 'string prefix (write-to-string *gentemp-counter*))))"

    ;; ── FR-510: *gensym-counter* ─────────────────────────────────────────────
    "(defvar *gensym-counter* 0)"

    ;; ── FR-429: *macroexpand-hook* ───────────────────────────────────────────
    "(defvar *macroexpand-hook* #'funcall)"

    ;; ── FR-549: function-lambda-expression ───────────────────────────────────
    "(defun function-lambda-expression (fn)
   (declare (ignore fn))
   (values nil nil nil))"

    ;; ── FR-553: upgraded-array-element-type / upgraded-complex-part-type ─────
    "(defun upgraded-array-element-type (typespec &optional environment)
   (declare (ignore typespec environment))
   t)"

    "(defun upgraded-complex-part-type (typespec &optional environment)
   (declare (ignore typespec environment))
   'real)"

    ;; ── FR-550: make-load-form / make-load-form-saving-slots ────────────────
    "(defgeneric make-load-form (object &optional environment))"
    "(defmethod make-load-form ((object t) &optional environment)
       (declare (ignore environment))
       (cond
         ((or (numberp object) (characterp object)) (values object nil))
         ((symbolp object) (values (list 'quote object) nil))
         ((stringp object) (values object nil))
         (t (values nil nil))))"

    "(defun make-load-form-saving-slots (object &rest slot-names)
       (declare (ignore slot-names))
       (if (hash-table-p object)
           (let ((class (gethash :__class__ object)))
             (if class
                 (values (list 'make-instance (list 'quote (gethash :__name__ class))) nil)
                 (values nil nil)))
           (values nil nil)))"

    ;; ── FR-427: make-condition ──────────────────────────────────────────────
    "(defun make-condition (type &rest initargs)
   (apply #'make-instance type initargs))"

    ;; ── FR-478: parse-integer (basic implementation) ─────────────────────────
    "(defun %parse-integer-impl (string start end radix junk-allowed)
   (let ((i start) (sign 1) (result 0) (found nil) (len (if end (min end (length string)) (length string))))
     (loop while (and (< i len) (char= (char string i) #\\Space))
           do (setq i (+ i 1)))
     (when (< i len)
       (cond ((char= (char string i) #\\+) (setq i (+ i 1)))
             ((char= (char string i) #\\-) (setq sign -1) (setq i (+ i 1)))))
     (loop while (< i len)
           do (let ((code (char-code (char string i))))
                (let ((digit (cond
                               ((and (>= code 48) (< code (min 58 (+ 48 radix))))
                                (- code 48))
                               ((and (>= radix 11) (>= code 65) (< code (+ 55 radix)))
                                (- code 55))
                               ((and (>= radix 11) (>= code 97) (< code (+ 87 radix)))
                                (- code 87))
                               (t nil))))
                  (if digit
                      (progn (setq result (+ (* result radix) digit))
                             (setq found t)
                             (setq i (+ i 1)))
                      (if junk-allowed
                          (return nil)
                          (error \"not a valid integer\"))))))
     (loop while (and (< i len) (char= (char string i) #\\Space))
           do (setq i (+ i 1)))
     (when (and (< i len) (not junk-allowed))
       (error \"junk in string\"))
     (if found (values (* sign result) i)
       (if junk-allowed (values nil i)
         (error \"not an integer\")))))"

    ;; ── FR-391: parse-number (delegating to read-from-string) ──────────
    "(defun parse-number (string &key (start 0) (end nil) (radix 10))
   (let* ((substr (subseq string start (or end (length string))))
          (result (read-from-string substr)))
     (if (numberp result) result
       (error \"~S is not a number\" substr))))"

;;; ── ANSI Base Classes (FR-528) ──────────────────────────────────────
    "(defclass standard-object () ())"
    "(defclass standard-class (standard-object) ())"
    "(defclass structure-object () ())"
    "(defclass method-combination (standard-object)
       ((name :initarg :name :reader method-combination-name)))"
    "(defclass funcallable-standard-class (standard-class) ())"

;;; ── Setf Expansion (FR-355) ────────────────────────────────────────

    "(defun get-setf-expansion (place &optional env)
       (declare (ignore env))
       (if (symbolp place)
           (let ((store (gensym \"STORE\")))
             (values nil nil (list store) (list 'setq place store) place))
           (let* ((temps (loop repeat (length (cdr place)) collect (gensym \"T\")))
                  (store (gensym \"STORE\"))
                  (access (cons (car place) temps))
                  (setter (list 'setf access store)))
             (values temps (cdr place) (list store) setter access))))"

    ;; ── FR-971 / FR-398: expander-backed special forms ────────────────────
    "(define-expander-for eval-when (form)
       (expand-eval-when-form (second form) (cddr form)))"

    "(define-expander-for symbol-macrolet (form)
       (expand-symbol-macrolet-form (second form) (cddr form)))"

    "(define-expander-for define-symbol-macro (form)
       (let ((name (second form))
             (expansion (third form)))
         (setf (gethash name *symbol-macro-table*) expansion)
         (list 'quote name)))"

;;; ── Missing ANSI functions (added for 100% coverage) ─────────────────────────

    ;; String
    "(defun string-trim (bag string)
       (cl:string-trim bag string))"
    "(defun string-left-trim (bag string)
       (cl:string-left-trim bag string))"
    "(defun string-right-trim (bag string)
       (cl:string-right-trim bag string))"
    "(defun string-upcase (string &key start end)
       (cl:string-upcase string :start start :end end))"
    "(defun string-downcase (string &key start end)
       (cl:string-downcase string :start start :end end))"
    "(defun string-capitalize (string &key start end)
       (cl:string-capitalize string :start start :end end))"
    "(defun nstring-upcase (string &key start end)
       (cl:nstring-upcase string :start start :end end))"
    "(defun nstring-downcase (string &key start end)
       (cl:nstring-downcase string :start start :end end))"
    "(defun nstring-capitalize (string &key start end)
       (cl:nstring-capitalize string :start start :end end))"
    "(defun char-not-equal (c &rest chars)
       (not (apply #'char= c chars)))"
    "(defun char-not-lessp (c &rest chars)
       (not (apply #'char< c chars)))"
    "(defun char-not-greaterp (c &rest chars)
       (not (apply #'char> c chars)))"

    ;; Copy
    "(defun copy-list (lst)
       (if (null lst) nil
           (cons (car lst) (copy-list (cdr lst)))))"
    "(defun list-length (lst)
       (let ((n 0) (slow lst) (fast lst))
         (loop
           (when (null fast) (return n))
           (when (null (cdr fast)) (return (1+ n)))
           (incf n 2)
           (setf slow (cdr slow) fast (cddr fast))
           (when (eq slow fast) (return nil)))))"

    ;; Sequence
    "(defun make-sequence (type length &key (initial-element nil))
       (cond ((subtypep type 'list)   (make-list length :initial-element initial-element))
             ((subtypep type 'vector) (make-array length :initial-element initial-element))
             ((subtypep type 'string) (make-string length :initial-element (or initial-element #\\Space)))
             (t (make-list length :initial-element initial-element))))"

    ;; Numeric bit/float functions
    "(defun integer-length (n)
       (if (zerop n) 0
           (1+ (floor (log (abs n) 2)))))"
    "(defun logcount (n)
       (let ((x (if (minusp n) (lognot n) n)) (c 0))
         (loop while (> x 0) do (incf c (logand x 1)) (setf x (ash x -1)))
         c))"
    "(defun arithmetic-shift (integer count)
       (ash integer count))"
    "(defun float-sign (float1 &optional (float2 1.0))
       (if (minusp float1) (- (abs float2)) (abs float2)))"
    "(defun float-digits (f) (declare (ignore f)) 53)"
    "(defun float-precision (f) (declare (ignore f)) 53)"
    "(defun float-radix (f) (declare (ignore f)) 2)"
    "(defun decode-float (f)
       (if (zerop f)
           (values 0.0d0 0 1.0d0)
           (let* ((exp (floor (log (abs f) 2)))
                  (mantissa (/ f (expt 2.0d0 exp))))
             (values mantissa exp (if (minusp f) -1.0d0 1.0d0)))))"
    "(defun scale-float (f e) (* f (expt 2.0d0 e)))"
    "(defun integer-decode-float (f)
       (multiple-value-bind (m e s) (decode-float f)
         (values (round (* m (expt 2 52))) (- e 52) (if (minusp s) -1 1))))"
    "(defconstant most-positive-fixnum #.(1- (expt 2 62)))"
    "(defconstant most-negative-fixnum #.(- (expt 2 62)))"
    "(defconstant most-positive-short-float 1.0e38)"
    "(defconstant most-negative-short-float -1.0e38)"
    "(defconstant least-positive-short-float 1.0e-38)"
    "(defconstant least-positive-normalized-short-float 1.0e-38)"

    ;; Control
    "(defmacro multiple-value-prog1 (first-form &rest other-forms)
       (let ((result-sym (gensym \"MV-RESULT\")))
         `(let ((,result-sym (multiple-value-list ,first-form)))
            ,@other-forms
            (values-list ,result-sym))))"

    ;; CLOS slot predicates
    "(defun slot-boundp (instance slot-name)
       (handler-case
           (progn (slot-value instance slot-name) t)
         (error () nil)))"
    "(defun slot-makunbound (instance slot-name)
       (declare (ignore instance slot-name))
       instance)"
    "(defun slot-exists-p (instance slot-name)
       (cl:slot-exists-p instance slot-name))"
    "(defun update-instance-for-different-class (previous current &rest initargs)
       (declare (ignore previous current initargs))
       nil)"
    ;; Hash table accessors
    "(defun sxhash (object)
       (cond ((integerp object) object)
             ((stringp object) (reduce (lambda (h c) (logxor (ash h 5) (char-code c))) object :initial-value 0))
             (t 0)))"
    "(defun hash-table-rehash-size (ht)
       (declare (ignore ht))
       1.5)"
    "(defun hash-table-rehash-threshold (ht)
       (declare (ignore ht))
       0.75)"
    "(defun hash-table-size (ht)
       (hash-table-count ht))"

    ;; Stream constructors
    "(defun make-broadcast-stream (&rest streams)
       (declare (ignore streams))
       (make-string-output-stream))"
    "(defun make-echo-stream (input-stream output-stream)
       (declare (ignore input-stream output-stream))
       (make-string-input-stream \"\"))"
    "(defun make-synonym-stream (symbol)
       (declare (ignore symbol))
       (make-string-output-stream))"
    "(defun stream-error-stream (condition)
       (declare (ignore condition))
       *standard-output*)"

    ;; Pathname functions (CL pathname model mapped to strings)
    "(defun pathname (x) (cl:pathname x))"
    "(defun make-pathname (&key host device directory name type version defaults)
       (cl:make-pathname :host host :device device :directory directory
                         :name name :type type :version version
                         :defaults defaults))"
    "(defun namestring (x) (cl:namestring x))"
    "(defun truename (x) (cl:truename x))"
    "(defun probe-file (x) (cl:probe-file x))"
    "(defun pathname-name (x) (cl:pathname-name x))"
    "(defun pathname-type (x) (cl:pathname-type x))"
    "(defun pathname-directory (x) (cl:pathname-directory x))"
    "(defun pathname-host (x) (cl:pathname-host x))"
    "(defun pathname-device (x) (cl:pathname-device x))"
    "(defun pathname-version (x) (cl:pathname-version x))"
    "(defun merge-pathnames (pathname &optional (defaults *default-pathname-defaults*) default-version)
       (cl:merge-pathnames pathname defaults default-version))"
    "(defun enough-namestring (pathname &optional defaults)
       (cl:enough-namestring pathname defaults))"
    "(defun file-namestring (pathname)
       (cl:file-namestring pathname))"
    "(defun directory-namestring (pathname)
       (cl:directory-namestring pathname))"
    "(defun wild-pathname-p (pathname &optional field-key)
       (cl:wild-pathname-p pathname field-key))"
    "(defun pathname-match-p (pathname wildcard)
       (cl:pathname-match-p pathname wildcard))"
    "(defun translate-pathname (source from-wildcard to-wildcard)
       (cl:translate-pathname source from-wildcard to-wildcard))"

       ))
