;;;; PHP serialization string builtins.

(in-package :cl-cc/php)

;;; ─── serialize / unserialize (PHP's native serialization format) ────────────
;;;
;;; N;                  null
;;; b:0; / b:1;         bool
;;; i:42;               int
;;; d:3.14;             float
;;; s:5:"hello";        string (LEN is byte length; we approximate with chars)
;;; a:2:{<k><v><k><v>}  array — count, then key/value serialized pairs
;;; O:1:"C":1:{<k><v>}  object — class name, count, then visible property pairs

(defun %php-serialize-float-text (val)
  "Render float VAL the way PHP serialize does: integral values without a
fractional part (3.0 -> \"3\"), others in full."
  (if (= val (floor val))
      (format nil "~D" (floor val))
      (format nil "~F" val)))

(defun %php-object-visible-pair-by-name (object name)
  "Return OBJECT's visible property pair named NAME, or NIL."
  (let ((name-string (%php-stringify name)))
    (find name-string (%php-object-visible-pairs object)
          :key (lambda (pair) (%php-stringify (car pair)))
          :test #'string=)))

(defun %php-object-sleep-property-names (value)
  "Normalize __sleep() VALUE to a Common Lisp list of property names."
  (cond
    ((hash-table-p value)
     (%php-array-values-list value))
    ((listp value)
     value)
    ((vectorp value)
     (coerce value 'list))
    (t nil)))

(defun %php-object-serialize-pairs (object)
  "Return the property pairs to serialize for OBJECT.

The object's visible properties are snapshotted BEFORE any magic-method lookup:
%php-object-method re-enters the class/instance machinery, and for
vector-backed instances that lookup can leave the instance's slot vector in a
state where a subsequent %php-object-visible-pairs re-scan reads back nothing.
Capturing VISIBLE up front makes serialization independent of that ordering."
  (let ((visible (%php-object-visible-pairs object))
        (serialize-hook (%php-object-method object "__serialize"))
        (sleep-hook (%php-object-method object "__sleep")))
    (cond
      (serialize-hook
       (let ((value (funcall serialize-hook object)))
         (if (hash-table-p value)
             (%php-array-pairs value)
             visible)))
      (sleep-hook
       (%php-trigger-error
        "PHP 8.5 deprecates __sleep(); use __serialize() instead"
        8192)
       (let ((names (%php-object-sleep-property-names (funcall sleep-hook object)))
             (pairs '()))
         (dolist (name names (nreverse pairs))
           (let ((pair (find (%php-stringify name) visible
                             :key (lambda (pair) (%php-stringify (car pair)))
                             :test #'string=)))
             (when pair
               (push pair pairs))))))
      (t visible))))

(defun %php-object-unserialize-payload (object)
  "Build the payload array passed to __unserialize()."
  (let ((payload (%php-make-array)))
    (dolist (pair (%php-array-pairs object) payload)
      (unless (string= (%php-stringify (car pair)) "__class__")
        (%php-array-set payload (car pair) (cdr pair))))))

(defun %php-object-handle-unserialize-hooks (object)
  "Invoke OBJECT's post-unserialize magic method, if any."
  (let ((unserialize-hook (%php-object-method object "__unserialize")))
    (if unserialize-hook
        (funcall unserialize-hook object (%php-object-unserialize-payload object))
        (let ((wakeup-hook (%php-object-method object "__wakeup")))
          (when wakeup-hook
            (%php-trigger-error
             "PHP 8.5 deprecates __wakeup(); use __unserialize() instead"
             8192)
            (funcall wakeup-hook object))))))

(defun %php-unserialize-runtime-state ()
  "Return the VM state active during host-runtime execution, or NIL."
  (or cl-cc/vm:*vm-state* cl-cc/vm:*vm-current-state*))

(defun %php-unserialize-class-descriptor (class-name)
  "Return the VM class descriptor for CLASS-NAME (a string), or NIL when the
class is unknown to the running program."
  (let ((state (%php-unserialize-runtime-state)))
    (when state
      (gethash (intern (string-upcase class-name) :cl-cc/php)
               (cl-cc/vm:vm-class-registry state)))))

(defun %php-unserialize-set-property (object key value)
  "Assign VALUE to property KEY on a reconstructed OBJECT (a vector-backed
instance or a __class__-tagged associative array)."
  (cond
    ((cl-cc/vm::%vm-vector-instance-p object)
     (let* ((class-ht (aref object 0))
            (sym (intern (string-upcase (%php-stringify key)) :cl-cc/php))
            (index (cl-cc/vm::%vm-slot-vector-index class-ht sym)))
       (when (and index (< index (length object)))
         (setf (aref object index) value))))
    ((hash-table-p object)
     (setf (gethash (%php-stringify key) object) value))))

(defun %php-unserialize-object (class-name data)
  "Reconstruct a PHP object of CLASS-NAME from DATA (a PHP array of property
key/value pairs), running the __unserialize/__wakeup magic methods as PHP does.

When the class is known, a real vector-backed instance is allocated so its
methods (stored as instance-slot closures) resolve and property access works;
%vm-raw-allocate-instance materializes those method closures and the declared
property defaults.  Unknown classes fall back to a __class__-tagged array so
malformed or foreign payloads still round-trip as data."
  (let ((class-ht (%php-unserialize-class-descriptor class-name)))
    (if (and (hash-table-p class-ht)
             (cl-cc/vm::%vm-fixed-slot-layout-p class-ht))
        (let* ((object (cl-cc/vm::%vm-raw-allocate-instance
                        class-ht nil (%php-unserialize-runtime-state)))
               (unserialize-hook (%php-object-method object "__unserialize")))
          (if unserialize-hook
              (funcall unserialize-hook object data)
              (progn
                (dolist (pair (%php-array-pairs data))
                  (%php-unserialize-set-property object (car pair) (cdr pair)))
                (let ((wakeup-hook (%php-object-method object "__wakeup")))
                  (when wakeup-hook
                    (%php-trigger-error
                     "PHP 8.5 deprecates __wakeup(); use __unserialize() instead"
                     8192)
                    (funcall wakeup-hook object)))))
          object)
        (let ((object (%php-make-array)))
          (setf (gethash "__class__" object) class-name)
          (dolist (pair (%php-array-pairs data))
            (setf (gethash (%php-stringify (car pair)) object) (cdr pair)))
          (%php-object-handle-unserialize-hooks object)
          object))))

(defun %php-serialize-into (value out)
  "Write the PHP-serialized form of VALUE to stream OUT."
  (cond
    ((%php-null-p value) (write-string "N;" out))
    ((eq value t) (write-string "b:1;" out))
    ((null value) (write-string "b:0;" out))
    ((integerp value) (format out "i:~D;" value))
    ((floatp value) (format out "d:~A;" (%php-serialize-float-text value)))
    ((stringp value) (format out "s:~D:\"~A\";" (length value) value))
    ((%php-object-class-name value)
     (let* ((class-name (%php-object-class-name value))
            (pairs (%php-object-serialize-pairs value)))
       (format out "O:~D:\"~A\":~D:{" (length class-name) class-name (length pairs))
       (dolist (pair pairs)
         (%php-serialize-into (%php-stringify (car pair)) out)
         (%php-serialize-into (cdr pair) out))
       (write-string "}" out)))
    ((hash-table-p value)
     (let ((pairs (%php-array-pairs value)))
       (format out "a:~D:{" (length pairs))
       (dolist (pair pairs)
         (%php-serialize-into (car pair) out)
         (%php-serialize-into (cdr pair) out))
       (write-string "}" out)))
    (t (write-string "N;" out))))

(defun %php-serialize (value)
  "PHP serialize: produce a storable string representation of VALUE."
  (with-output-to-string (out)
    (%php-serialize-into value out)))

(defun %php-unserialize-at (str pos)
  "Parse one serialized value from STR at POS. Return (values value next-pos)."
  (case (char str pos)
    (#\N (values +php-null+ (+ pos 2)))                         ; N;
    (#\b (values (char= (char str (+ pos 2)) #\1) (+ pos 4)))   ; b:0; / b:1;
    (#\i (let* ((start (+ pos 2)) (semi (position #\; str :start start)))
           (values (parse-integer str :start start :end semi) (1+ semi))))
    (#\d (let* ((start (+ pos 2)) (semi (position #\; str :start start)))
           (values (let ((*read-default-float-format* 'double-float))
                     (coerce (read-from-string (subseq str start semi)) 'double-float))
                   (1+ semi))))
    (#\s (let* ((len-start (+ pos 2))
                (colon (position #\: str :start len-start))
                (len (parse-integer str :start len-start :end colon))
                (data-start (+ colon 2))            ; skip :"
                (data-end (+ data-start len)))
           (values (subseq str data-start data-end) (+ data-end 2)))) ; skip ";
    (#\a (let* ((count-start (+ pos 2))
                (colon (position #\: str :start count-start))
                (count (parse-integer str :start count-start :end colon))
                (p (+ colon 2))                     ; skip :{
                (arr (%php-make-array)))
           (dotimes (_ count)
             (multiple-value-bind (k kp) (%php-unserialize-at str p)
               (multiple-value-bind (v vp) (%php-unserialize-at str kp)
                 (%php-array-set arr k v)
                 (setf p vp))))
           (values arr (1+ p))))                    ; skip }
    (#\O (let* ((class-start (+ pos 2))
                (class-len-end (position #\: str :start class-start))
                (class-len (parse-integer str :start class-start :end class-len-end))
                (class-quote-start (+ class-len-end 2))
                (class-name (subseq str class-quote-start (+ class-quote-start class-len)))
                (count-start (+ class-quote-start class-len 2))
                (count-end (position #\: str :start count-start))
                (count (parse-integer str :start count-start :end count-end))
                (p (+ count-end 2))
                (data (%php-make-array)))
           (dotimes (_ count)
             (multiple-value-bind (k kp) (%php-unserialize-at str p)
               (multiple-value-bind (v vp) (%php-unserialize-at str kp)
                 (%php-array-set data k v)
                 (setf p vp))))
           (values (%php-unserialize-object class-name data) (1+ p)))) ; skip }
    ;; Unknown tag -> malformed input; PHP unserialize returns false (NIL).
    (t (values nil pos))))

(defun %php-unserialize (str)
  "PHP unserialize: reconstruct a value from its serialized STR. Returns false
(NIL) on malformed input, like PHP."
  (handler-case
      (multiple-value-bind (value pos) (%php-unserialize-at (%php-stringify str) 0)
        (declare (ignore pos))
        value)
    (error () nil)))
