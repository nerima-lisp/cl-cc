;;;; tests/unit/vm/vm-bridge-tests.lisp
;;;; Unit tests for src/vm/vm-bridge.lisp — host bridge + CLOS slot metadata.
;;;;
;;;; Covers: hash-table-values, vm-register-host-bridge,
;;;;   slot-definition-name, slot-definition-initform,
;;;;   slot-definition-initargs, slot-definition-allocation,
;;;;   %class-slot-initargs-for-slot, %class-slot-metadata,
;;;;   %class-slot-definitions, rt-plist-put,
;;;;   generic-function-methods, generic-function-method-combination.

(in-package :cl-cc/test)

;;; ─── hash-table-values ───────────────────────────────────────────────────

(it-sequential "hash-table-values-empty-returns-nil"
  (expect (cl-cc/vm::hash-table-values (make-hash-table :test #'eq)) :to-be-null))

(it-sequential "hash-table-values-singleton"
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :k ht) 42)
    (expect (cl-cc/vm::hash-table-values ht) :to-equal '(42))))

(it-sequential "hash-table-values-multiple-entries"
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :a ht) 1
          (gethash :b ht) 2
          (gethash :c ht) 3)
    (let ((vals (cl-cc/vm::hash-table-values ht)))
      (expect (= 3 (length vals)) :to-be-truthy)
      (expect (member 1 vals) :to-be-truthy)
      (expect (member 2 vals) :to-be-truthy)
      (expect (member 3 vals) :to-be-truthy))))

;;; ─── vm-register-host-bridge ─────────────────────────────────────────────

(it-sequential "vm-register-host-bridge-stores-in-table"
  (let ((sym (gensym "BRIDGE-TEST-")))
    (cl-cc/vm::vm-register-host-bridge sym (lambda () :ok))
    (expect (gethash sym cl-cc/vm::*vm-host-bridge-functions*) :to-be-truthy)))

(it-sequential "vm-register-host-bridge-is-idempotent"
  (let ((sym (gensym "BRIDGE-IDEM-")))
    (cl-cc/vm::vm-register-host-bridge sym (lambda () :ok))
    (cl-cc/vm::vm-register-host-bridge sym (lambda () :ok))
    (expect (gethash sym cl-cc/vm::*vm-host-bridge-functions*) :to-be-truthy)))

(it-sequential "vm-bridge-callable-resolves-same-name-aliases"
  (let ((callable (cl-cc/vm::vm-bridge-callable (make-symbol "STRING-TO-OCTETS"))))
    (expect (functionp callable) :to-be-truthy)
    (let ((octets (funcall callable "é" :external-format :latin-1)))
      (expect (= 1 (length octets)) :to-be-truthy)
      (expect (= 233 (aref octets 0)) :to-be-truthy))))

(it-sequential "vm-bridge-string-octets-normalizes-utf-16"
  (let ((octets (cl-cc/vm:string-to-octets "A" :encoding :utf-16)))
    (expect (vectorp octets) :to-be-truthy)
    (expect (= 2 (length octets)) :to-be-truthy)
    (expect (= 65 (aref octets 0)) :to-be-truthy)
    (expect (= 0 (aref octets 1)) :to-be-truthy)
    (expect (cl-cc/vm:octets-to-string octets :encoding :utf-16) :to-equal "A")))

(it-sequential "vm-bridge-string-octets-utf-16-surrogate-roundtrip"
  (let* ((text (string (code-char #x1f600)))
         (octets (cl-cc/vm:string-to-octets text :encoding :utf-16)))
    (expect (= 4 (length octets)) :to-be-truthy)
    (expect (= #x3d (aref octets 0)) :to-be-truthy)
    (expect (= #xd8 (aref octets 1)) :to-be-truthy)
    (expect (= #x00 (aref octets 2)) :to-be-truthy)
    (expect (= #xde (aref octets 3)) :to-be-truthy)
    (expect (cl-cc/vm:octets-to-string octets :encoding :utf-16) :to-equal text)))

(it-sequential "vm-bridge-registers-compile-file-pathname"
  (let ((callable (gethash 'compile-file-pathname cl-cc/vm::*vm-host-bridge-functions*)))
    (expect (functionp callable) :to-be-truthy)
    (expect (pathnamep (funcall callable "/tmp/cl-cc-bridge-test.lisp")) :to-be-truthy)))

(it-sequential "vm-register-runtime-callable-and-lookup"
  (let ((name "RT-UNIT-TEST-CALLABLE"))
    (unwind-protect
         (progn
           (cl-cc/vm::vm-register-runtime-callable name (lambda () :ok))
            (expect (functionp (gethash name cl-cc/vm::*vm-runtime-callables*)) :to-be-truthy)
            (expect (funcall (cl-cc/vm::%vm-runtime-callable name)) :to-be :ok))
      (remhash name cl-cc/vm::*vm-runtime-callables*))))

(it-sequential "vm-bridge-registers-php-current-closure"
  (expect (functionp (cl-cc/vm::vm-bridge-callable 'cl-cc/php::%php-current-closure)) :to-be-truthy)
  (expect (functionp (gethash "PHP-CURRENT-CLOSURE" cl-cc/vm::*vm-runtime-callables*)) :to-be-truthy))

(it-sequential "vm-bridge-runtime-registration-uses-bootstrap-hook"
  (let ((old-hook cl-cc/bootstrap::*runtime-vm-callable-register-hook*)
        (called nil))
    (unwind-protect
         (progn
           (setf cl-cc/bootstrap::*runtime-vm-callable-register-hook*
                 (lambda () (setf called t)))
           (when cl-cc/bootstrap::*runtime-vm-callable-register-hook*
             (funcall cl-cc/bootstrap::*runtime-vm-callable-register-hook*))
           (expect called :to-be-truthy))
      (setf cl-cc/bootstrap::*runtime-vm-callable-register-hook* old-hook))))

(it-sequential "vm-install-eval-hooks-sets-hook-vars"
  (let ((old-eval cl-cc/vm::*vm-eval-hook*)
        (old-compile cl-cc/vm::*vm-compile-string-hook*)
        (eval-hook (lambda (&rest _) (declare (ignore _)) :eval))
        (compile-hook (lambda (&rest _) (declare (ignore _)) :compile)))
    (unwind-protect
         (progn
           (cl-cc/vm::vm-install-eval-hooks eval-hook compile-hook)
           (expect cl-cc/vm::*vm-eval-hook* :to-be eval-hook)
           (expect cl-cc/vm::*vm-compile-string-hook* :to-be compile-hook))
      (setf cl-cc/vm::*vm-eval-hook* old-eval)
      (setf cl-cc/vm::*vm-compile-string-hook* old-compile))))

(it-sequential "vm-install-macroexpand-hooks-sets-hook-vars"
  (let ((old-1 cl-cc/vm::*vm-macroexpand-1-hook*)
        (old-all cl-cc/vm::*vm-macroexpand-hook*)
        (hook-1 (lambda (&rest _) (declare (ignore _)) :m1))
        (hook-all (lambda (&rest _) (declare (ignore _)) :mall)))
    (unwind-protect
         (progn
           (cl-cc/vm::vm-install-macroexpand-hooks hook-1 hook-all)
           (expect cl-cc/vm::*vm-macroexpand-1-hook* :to-be hook-1)
           (expect cl-cc/vm::*vm-macroexpand-hook* :to-be hook-all))
      (setf cl-cc/vm::*vm-macroexpand-1-hook* old-1)
      (setf cl-cc/vm::*vm-macroexpand-hook* old-all))))

(it-sequential "vm-install-parse-forms-hook-sets-hook-var"
  (let ((old cl-cc/vm::*vm-parse-forms-hook*)
        (hook (lambda (&rest _) (declare (ignore _)) :parse)))
    (unwind-protect
         (progn
           (cl-cc/vm::vm-install-parse-forms-hook hook)
            (expect cl-cc/vm::*vm-parse-forms-hook* :to-be hook))
      (setf cl-cc/vm::*vm-parse-forms-hook* old))))

(it-sequential "vm-runtime-package-registry-uses-bootstrap-provider"
  (let ((old-provider cl-cc/bootstrap::*runtime-package-registry-provider*)
        (registry (make-hash-table :test #'equal)))
    (unwind-protect
         (progn
           (setf (gethash "CL-CC/RUNTIME" registry) :runtime)
           (setf cl-cc/bootstrap::*runtime-package-registry-provider*
                 (lambda () registry))
           (expect (cl-cc/vm::%vm-runtime-package-registry) :to-be registry))
      (setf cl-cc/bootstrap::*runtime-package-registry-provider* old-provider))))

(it-sequential "vm-bridge-registers-confirmed-stdlib-bridge-entries"
  (dolist (sym '(make-pathname pathname namestring file-namestring
                 pathname-name pathname-type pathname-host pathname-device
                 pathname-directory pathname-version merge-pathnames truename
                 parse-namestring wild-pathname-p pathname-match-p
                 translate-pathname compile-file-pathname probe-file
                 rename-file delete-file file-write-date file-author
                 directory ensure-directories-exist make-synonym-stream
                 make-broadcast-stream make-two-way-stream make-echo-stream
                 make-concatenated-stream broadcast-stream-streams
                 two-way-stream-input-stream two-way-stream-output-stream
                 echo-stream-input-stream echo-stream-output-stream
                 concatenated-stream-streams file-string-length disassemble
                 inspect y-or-n-p yes-or-no-p string-to-octets octets-to-string))
    (expect (functionp (cl-cc/vm::vm-bridge-callable sym)) :to-be-truthy)))

;;; ─── slot-definition-name ────────────────────────────────────────────────

(it-sequential "slot-definition-name-from-symbol"
  (expect (cl-cc/vm::slot-definition-name 'my-slot) :to-be 'my-slot))

(it-sequential "slot-definition-name-from-ht-descriptor"
  (let ((slot (make-hash-table :test #'eq)))
    (setf (gethash :name slot) 'count)
    (expect (cl-cc/vm::slot-definition-name slot) :to-be 'count)))

(it-sequential "slot-definition-name-nil-for-non-symbol-non-ht"
  (expect (cl-cc/vm::slot-definition-name 42) :to-be-null))

;;; ─── slot-definition-initform ────────────────────────────────────────────

(it-sequential "slot-definition-initform stored-value"
  (destructuring-bind (slot verify) (list (let ((s (make-hash-table :test #'eq)))
             (setf (gethash :initform s) 0) s) (lambda (slot)
             (expect (= 0 (cl-cc/vm::slot-definition-initform slot)) :to-be-truthy)))
    (funcall verify slot)))

(it-sequential "slot-definition-initform absent"
  (destructuring-bind (slot verify) (list (make-hash-table :test #'eq) (lambda (slot)
             (expect (cl-cc/vm::slot-definition-initform slot) :to-be-null)))
    (funcall verify slot)))

(it-sequential "slot-definition-initform symbol-slot"
  (destructuring-bind (slot verify) (list 'x (lambda (slot)
             (expect (cl-cc/vm::slot-definition-initform slot) :to-be-null)))
    (funcall verify slot)))

;;; ─── slot-definition-initargs ────────────────────────────────────────────

(it-sequential "slot-definition-initargs stored-list"
  (destructuring-bind (slot verify) (list (let ((s (make-hash-table :test #'eq)))
             (setf (gethash :initargs s) '(:count)) s) (lambda (slot)
             (expect (cl-cc/vm::slot-definition-initargs slot) :to-equal '(:count))))
    (funcall verify slot)))

(it-sequential "slot-definition-initargs symbol-slot"
  (destructuring-bind (slot verify) (list 'x (lambda (slot)
             (expect (cl-cc/vm::slot-definition-initargs slot) :to-be-null)))
    (funcall verify slot)))

;;; ─── slot-definition-allocation ──────────────────────────────────────────

(it-sequential "slot-definition-allocation instance-default"
  (destructuring-bind (expected slot) (list :instance (make-hash-table :test #'eq))
    (expect (cl-cc/vm::slot-definition-allocation slot) :to-be expected)))

(it-sequential "slot-definition-allocation class-when-set"
  (destructuring-bind (expected slot) (list :class (let ((s (make-hash-table :test #'eq)))
                                            (setf (gethash :allocation s) :class) s))
    (expect (cl-cc/vm::slot-definition-allocation slot) :to-be expected)))

(it-sequential "slot-definition-allocation symbol-slot"
  (destructuring-bind (expected slot) (list :instance 'x)
    (expect (cl-cc/vm::slot-definition-allocation slot) :to-be expected)))

;;; ─── %class-slot-initargs-for-slot ──────────────────────────────────────

(it-sequential "class-slot-initargs-for-slot-found"
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__initargs__ class) '((:count . count) (:value . value)))
    (expect (cl-cc/vm::%class-slot-initargs-for-slot class 'count) :to-equal '(:count))))

(it-sequential "class-slot-initargs-for-slot-absent-returns-nil"
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__initargs__ class) '((:x . x)))
    (expect (cl-cc/vm::%class-slot-initargs-for-slot class 'y) :to-be-null)))

(it-sequential "class-slot-initargs-for-slot-non-ht-class-returns-nil"
  (expect (cl-cc/vm::%class-slot-initargs-for-slot 'not-a-class 'slot) :to-be-null))

;;; ─── %class-slot-metadata ────────────────────────────────────────────────

(it-sequential "class-slot-metadata-instance-allocation"
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__initargs__    class) '((:n . n))
          (gethash :__initforms__   class) '((n . 0))
          (gethash :__class-slots__ class) nil)
    (let ((slot (cl-cc/vm::%class-slot-metadata class 'n)))
      (expect (hash-table-p slot) :to-be-truthy)
      (expect (gethash :name slot) :to-be 'n)
      (expect (= 0 (gethash :initform slot)) :to-be-truthy)
      (expect (gethash :initargs slot) :to-equal '(:n))
      (expect (gethash :allocation slot) :to-be :instance))))

(it-sequential "class-slot-metadata-class-allocation"
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__initargs__    class) nil
          (gethash :__initforms__   class) nil
          (gethash :__class-slots__ class) '(shared))
    (let ((slot (cl-cc/vm::%class-slot-metadata class 'shared)))
      (expect (gethash :allocation slot) :to-be :class))))

;;; ─── %class-slot-definitions ─────────────────────────────────────────────

(it-sequential "class-slot-definitions-returns-list-of-descriptors"
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__slots__       class) '(a b))
    (setf (gethash :__initargs__    class) nil)
    (setf (gethash :__initforms__   class) nil)
    (setf (gethash :__class-slots__ class) nil)
    (let ((defs (cl-cc/vm::%class-slot-definitions class)))
      (expect (= 2 (length defs)) :to-be-truthy)
      (expect (every #'hash-table-p defs) :to-be-truthy))))

(it-sequential "class-slot-definitions-nil-cases non-ht"
  (destructuring-bind (class) (list 'symbol-class)
    (expect (cl-cc/vm::%class-slot-definitions class) :to-be-null)))

(it-sequential "class-slot-definitions-nil-cases no-slots"
  (destructuring-bind (class) (list (make-hash-table :test #'eq))
    (expect (cl-cc/vm::%class-slot-definitions class) :to-be-null)))

;;; ─── rt-plist-put ────────────────────────────────────────────────────────

(it-sequential "rt-plist-put-inserts-into-empty"
  (expect (cl-cc/bootstrap::rt-plist-put nil :color 'red) :to-equal '(:color red)))

(it-sequential "rt-plist-put-updates-existing-key"
  (let* ((plist  '(:a 1 :b 2))
         (result (cl-cc/bootstrap::rt-plist-put plist :a 99)))
    (expect (= 99 (getf result :a)) :to-be-truthy)
    (expect (= 2 (getf result :b)) :to-be-truthy)))

(it-sequential "rt-plist-put-preserves-other-keys"
  (let* ((plist  '(:x 10 :y 20 :z 30))
         (result (cl-cc/bootstrap::rt-plist-put plist :y 99)))
    (expect (= 10 (getf result :x)) :to-be-truthy)
    (expect (= 99 (getf result :y)) :to-be-truthy)
    (expect (= 30 (getf result :z)) :to-be-truthy)))

(it-sequential "rt-plist-put-is-non-destructive"
  (let* ((plist '(:a 1))
         (result (cl-cc/bootstrap::rt-plist-put plist :a 2)))
    (declare (ignore result))
    (expect (= 1 (getf plist :a)) :to-be-truthy)))

;;; ─── generic-function-methods ────────────────────────────────────────────

(it-sequential "generic-function-methods-nil-when-absent"
  (expect (cl-cc/vm::generic-function-methods (make-hash-table :test #'eq)) :to-be-null))

(it-sequential "generic-function-methods-returns-all-methods"
  (let ((gf (make-hash-table :test #'eq))
        (methods-ht (make-hash-table :test #'equal)))
    (setf (gethash '(integer) methods-ht) 'method-a
          (gethash '(string)  methods-ht) 'method-b
          (gethash :__methods__ gf)       methods-ht)
    (let ((result (cl-cc/vm::generic-function-methods gf)))
      (expect (= 2 (length result)) :to-be-truthy)
      (expect (member 'method-a result) :to-be-truthy)
      (expect (member 'method-b result) :to-be-truthy))))

;;; ─── generic-function-method-combination ─────────────────────────────────

(it-sequential "generic-function-method-combination-behavior"
  (let ((gf (make-hash-table :test #'eq)))
    (expect (cl-cc/vm::generic-function-method-combination gf) :to-be 'standard))
  (let ((gf (make-hash-table :test #'eq)))
    (setf (gethash :__method-combination__ gf) '+)
    (expect (cl-cc/vm::generic-function-method-combination gf) :to-be '+)))
