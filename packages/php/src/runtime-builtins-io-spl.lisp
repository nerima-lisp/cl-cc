;;;; PHP SPL runtime object helpers and data structures.

(in-package :cl-cc/php)

;;; ─── SPL data structures ────────────────────────────────────────────────────

(defun %php-spl-class-name (name)
  (let ((s (%php-stringify name)))
    (cond
      ((string-equal s "SplStack") "SplStack")
      ((string-equal s "SplQueue") "SplQueue")
      ((string-equal s "SplDoublyLinkedList") "SplDoublyLinkedList")
      ((string-equal s "SplMinHeap") "SplMinHeap")
      ((string-equal s "SplMaxHeap") "SplMaxHeap")
      ((string-equal s "SplFixedArray") "SplFixedArray")
      (t s))))

(defun %php-spl-index (value)
  (truncate (if (numberp value)
                value
                (%php-to-number (%php-stringify value)))))

(defun %php-spl-number (value)
  (if (numberp value)
      value
      (%php-to-number (%php-stringify value))))

(defun %php-spl-make-methods (&rest names)
  (let ((methods (%php-make-array)))
    (dolist (name names methods)
      (%php-array-set methods name name))))

(defun %php-spl-method-symbol (name)
  (intern (string-upcase name) :cl-cc/php))

(defun %php-spl-set-method (object name function)
  (setf (gethash name object) function
        (gethash (%php-spl-method-symbol name) object) function)
  function)

(defun %php-spl-install-methods (object specs)
  (dolist (spec specs object)
    (%php-spl-set-method object (first spec) (symbol-function (second spec)))))

(defun %php-spl-object (class-name methods)
  (let ((obj (make-hash-table :test #'equal)))
    (setf (gethash "__class__" obj) class-name)
    (setf (gethash "__methods__" obj) methods)
    obj))

(defun %php-spl-set-property (object name value)
  "Set a PHP-visible property on an SPL-style runtime object."
  (setf (gethash name object) value)
  (let ((fallback-name (string-downcase name)))
    (unless (string= fallback-name name)
      (setf (gethash fallback-name object) value)))
  value)


(defun %php-spl-items (object)
  (or (gethash "__items__" object) '()))

(defun %php-spl-set-items (object items)
  (setf (gethash "__items__" object) items))

(defun %php-spl-list-count (self)
  (length (%php-spl-items self)))

(defun %php-spl-list-empty-p (self)
  (zerop (%php-spl-list-count self)))

(defun %php-spl-list-push (self value)
  (%php-spl-set-items self (append (%php-spl-items self) (list value)))
  +php-null+)

(defun %php-spl-list-unshift (self value)
  (%php-spl-set-items self (cons value (%php-spl-items self)))
  +php-null+)

(defun %php-spl-list-pop (self)
  (let ((items (%php-spl-items self)))
    (if items
        (prog1 (car (last items))
          (%php-spl-set-items self (butlast items)))
        +php-null+)))

(defun %php-spl-list-shift (self)
  (let ((items (%php-spl-items self)))
    (if items
        (prog1 (first items)
          (%php-spl-set-items self (rest items)))
        +php-null+)))

(defun %php-spl-list-top (self)
  (let ((items (%php-spl-items self)))
    (if items (car (last items)) +php-null+)))

(defun %php-spl-list-bottom (self)
  (let ((items (%php-spl-items self)))
    (if items (first items) +php-null+)))

(defun %php-spl-list-queue-pop (self)
  (%php-spl-list-shift self))

(defparameter +php-spl-list-methods+
  '(("push" %php-spl-list-push)
    ("pop" %php-spl-list-pop)
    ("unshift" %php-spl-list-unshift)
    ("shift" %php-spl-list-shift)
    ("top" %php-spl-list-top)
    ("bottom" %php-spl-list-bottom)
    ("count" %php-spl-list-count)
    ("isEmpty" %php-spl-list-empty-p)))

(defparameter +php-spl-queue-methods+
  '(("push" %php-spl-list-push)
    ("pop" %php-spl-list-queue-pop)
    ("unshift" %php-spl-list-unshift)
    ("shift" %php-spl-list-shift)
    ("top" %php-spl-list-top)
    ("bottom" %php-spl-list-bottom)
    ("count" %php-spl-list-count)
    ("isEmpty" %php-spl-list-empty-p)
    ("enqueue" %php-spl-list-push)
    ("dequeue" %php-spl-list-shift)))

(defun %php-spl-install-list-methods (object &key queue-p)
  (%php-spl-install-methods object
                            (if queue-p
                                +php-spl-queue-methods+
                                +php-spl-list-methods+)))

(defun %php-spl-doubly-linked-list ()
  (let ((object (%php-spl-object
                 "SplDoublyLinkedList"
                 (%php-spl-make-methods "push" "pop" "unshift" "shift"
                                        "top" "bottom" "count" "isEmpty"))))
    (%php-spl-set-items object '())
    (%php-spl-install-list-methods object)))

(defun %php-spl-stack ()
  (let ((object (%php-spl-object
                 "SplStack"
                 (%php-spl-make-methods "push" "pop" "unshift" "shift"
                                        "top" "bottom" "count" "isEmpty"))))
    (%php-spl-set-items object '())
    (%php-spl-install-list-methods object)))

(defun %php-spl-queue ()
  (let ((object (%php-spl-object
                 "SplQueue"
                 (%php-spl-make-methods "enqueue" "dequeue" "push" "pop"
                                        "top" "bottom" "count" "isEmpty"))))
    (%php-spl-set-items object '())
    (%php-spl-install-list-methods object :queue-p t)))

(defun %php-spl-value< (left right)
  (let ((ln (%php-spl-number left))
        (rn (%php-spl-number right)))
    (cond
      ((or (/= ln 0) (/= rn 0)
           (member left '(0 0.0) :test #'eql)
           (member right '(0 0.0) :test #'eql))
       (< ln rn))
      (t (string< (%php-stringify left) (%php-stringify right))))))

(defun %php-spl-heap-best (items min-p)
  (reduce (lambda (best value)
            (if (if min-p
                    (%php-spl-value< value best)
                    (%php-spl-value< best value))
                value
                best))
          items))

(defun %php-spl-heap-remove-one (items target)
  (let ((removed nil))
    (loop for value in items
          unless (and (not removed) (equal value target))
            collect value
          else do (setf removed t))))

(defun %php-spl-heap-min-p (self)
  (gethash "__min_heap__" self))

(defun %php-spl-heap-insert (self value)
  (%php-spl-list-push self value))

(defun %php-spl-heap-top (self)
  (let ((items (%php-spl-items self)))
    (if items
        (%php-spl-heap-best items (%php-spl-heap-min-p self))
        +php-null+)))

(defun %php-spl-heap-extract (self)
  (let ((items (%php-spl-items self)))
    (if items
        (let ((best (%php-spl-heap-best items (%php-spl-heap-min-p self))))
          (%php-spl-set-items self (%php-spl-heap-remove-one items best))
          best)
        +php-null+)))

(defparameter +php-spl-heap-methods+
  '(("insert" %php-spl-heap-insert)
    ("extract" %php-spl-heap-extract)
    ("top" %php-spl-heap-top)
    ("count" %php-spl-list-count)
    ("isEmpty" %php-spl-list-empty-p)))

(defun %php-spl-heap (class-name min-p)
  (let ((object (%php-spl-object
                 class-name
                 (%php-spl-make-methods "insert" "extract" "top" "count" "isEmpty"))))
    (%php-spl-set-items object '())
    (setf (gethash "__min_heap__" object) min-p)
    (%php-spl-install-methods object +php-spl-heap-methods+)
    object))

(defun %php-spl-min-heap ()
  (%php-spl-heap "SplMinHeap" t))

(defun %php-spl-max-heap ()
  (%php-spl-heap "SplMaxHeap" nil))

(defun %php-spl-fixed-values (self)
  (or (gethash "__values__" self) '()))

(defun %php-spl-fixed-set-values (self values)
  (setf (gethash "__values__" self) values))

(defun %php-spl-fixed-normalize-size (size)
  (max 0 (%php-spl-index size)))

(defun %php-spl-fixed-resize-list (values size)
  (let ((current (length values)))
    (cond
      ((= current size) values)
      ((> current size) (subseq values 0 size))
      (t (append values (make-list (- size current) :initial-element +php-null+))))))

(defun %php-spl-fixed-ref (self index)
  (let* ((values (%php-spl-fixed-values self))
         (i (%php-spl-index index)))
    (if (and (>= i 0) (< i (length values)))
        (nth i values)
        +php-null+)))

(defun %php-spl-fixed-set (self index value)
  (let* ((values (%php-spl-fixed-values self))
         (i (%php-spl-index index)))
    (when (and (>= i 0) (< i (length values)))
      (setf (nth i values) value)
      (%php-spl-fixed-set-values self values))
    +php-null+))

(defun %php-spl-fixed-size (self)
  (length (%php-spl-fixed-values self)))

(defun %php-spl-fixed-set-size (self new-size)
  (%php-spl-fixed-set-values
   self
   (%php-spl-fixed-resize-list
    (%php-spl-fixed-values self)
    (%php-spl-fixed-normalize-size new-size)))
  +php-null+)

(defun %php-spl-fixed-offset-exists-p (self index)
  (let ((i (%php-spl-index index)))
    (and (>= i 0) (< i (%php-spl-fixed-size self)))))

(defun %php-spl-fixed-unset (self index)
  (%php-spl-fixed-set self index +php-null+))

(defparameter +php-spl-fixed-array-methods+
  '(("getSize" %php-spl-fixed-size)
    ("setSize" %php-spl-fixed-set-size)
    ("offsetGet" %php-spl-fixed-ref)
    ("offsetSet" %php-spl-fixed-set)
    ("offsetExists" %php-spl-fixed-offset-exists-p)
    ("offsetUnset" %php-spl-fixed-unset)
    ("count" %php-spl-fixed-size)))

(defun %php-spl-fixed-array (&optional size)
  (let ((object (%php-spl-object
                 "SplFixedArray"
                 (%php-spl-make-methods "getSize" "setSize" "offsetGet"
                                        "offsetSet" "offsetExists" "offsetUnset"
                                        "count"))))
    (%php-spl-fixed-set-values object
                               (make-list (%php-spl-fixed-normalize-size (or size 0))
                                          :initial-element +php-null+))
    (%php-spl-install-methods object +php-spl-fixed-array-methods+)
    object))

(defun %php-spl-new (class-name &rest args)
  (case (intern (string-upcase (%php-spl-class-name class-name)) :keyword)
    (:SPLSTACK (%php-spl-stack))
    (:SPLQUEUE (%php-spl-queue))
    (:SPLDOUBLYLINKEDLIST (%php-spl-doubly-linked-list))
    (:SPLMINHEAP (%php-spl-min-heap))
    (:SPLMAXHEAP (%php-spl-max-heap))
    (:SPLFIXEDARRAY (apply #'%php-spl-fixed-array args))
    (otherwise (error "Unknown SPL class: ~A" class-name))))
