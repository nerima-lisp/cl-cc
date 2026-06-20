;;;; packages/javascript/src/runtime-collections.lisp — JS Set and Iterator built-ins
;;;;
;;;; Set is represented as a js-set struct with a hash-table and an
;;;; insertion-order list (for deterministic for-of iteration). Public Set
;;;; operations canonicalize through the insertion-order values because
;;;; ECMAScript membership uses SameValueZero, which CL hash-table tests cannot
;;;; express exactly for NaN.
;;;; Iterators are represented as closures or JS objects with a "next" method.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Set built-ins (ES2025)
;;; -----------------------------------------------------------------------

;;; JS Set is represented as a struct with:
;;;   HT    — CL hash-table (key → t) for canonical stored values
;;;   ORDER — list of values in insertion order (used by for-of / keys / values)
(defstruct (js-set (:conc-name js-set-))
  (ht    (make-hash-table :test #'equal :size 8) :type hash-table)
  (order nil))

(defun %js-make-set ()
  "Create a new empty ordered JS Set."
  (make-js-set))

;;; Internal helper: copy every key from SRC (a js-set) into TARGET (a js-set).
(defun %js-set-copy-all (target src)
  (dolist (k (js-set-order src))
    (%js-set-add target k)))

(defun %js-set-find-value (s val)
  "Return the stored set value matching VAL by SameValueZero, plus found-p."
  (loop for stored-value in (js-set-order s)
        when (%js-same-value-zero stored-value val)
          return (values stored-value t)
        finally (return (values nil nil))))

(defun %js-set-add (s val)
  (unless (nth-value 1 (%js-set-find-value s val))
    (setf (gethash val (js-set-ht s)) t)
    (setf (js-set-order s) (append (js-set-order s) (list val))))
  s)

(defun %js-set-delete (s val)
  (multiple-value-bind (stored-value found-p) (%js-set-find-value s val)
    (when found-p
      (remhash stored-value (js-set-ht s))
      (setf (js-set-order s)
            (delete stored-value (js-set-order s)
                    :test #'%js-same-value-zero
                    :count 1)))
    found-p))

(defun %js-set-has (s val)
  (nth-value 1 (%js-set-find-value s val)))

(defun %js-set-clear (s)
  (clrhash (js-set-ht s))
  (setf (js-set-order s) nil)
  +js-undefined+)

(defun %js-set-size (s)
  (length (js-set-order s)))

(defun %js-set-keys (s)
  "Return a vector of the set's values in insertion order."
  (let ((order (js-set-order s)))
    (make-array (length order) :element-type t :adjustable t :fill-pointer (length order)
                :initial-contents order)))

(defun %js-set-entries (s)
  "Return array of [key, key] pairs (Set semantics)."
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (dolist (k (js-set-order s))
      (vector-push-extend (%js-make-array k k) result))
    result))

(defun %js-set-for-each (s fn)
  (dolist (k (js-set-order s))
    (%js-funcall fn k k s))
  +js-undefined+)

(defun %js-set-like-size (obj)
  (if (js-set-p obj)
      (%js-set-size obj)
      (%js-to-number (%js-get-prop obj "size"))))

(defun %js-set-like-has (obj value)
  (if (js-set-p obj)
      (%js-set-has obj value)
      (%js-truthy (%js-funcall (%js-get-prop obj "has") value))))

(defun %js-set-like-keys (obj)
  (if (js-set-p obj)
      (copy-list (js-set-order obj))
      (let ((size (%js-set-like-size obj))
            (keys nil))
        (declare (ignore size))
        (%js-for-of (%js-funcall (%js-get-prop obj "keys"))
                    (lambda (value)
                      (push value keys)
                      +js-undefined+))
        (nreverse keys))))

;;; Data-driven set operations
;;; (define-js-set-predicate name stop-condition result-if-stopped default-result)
(defmacro define-js-set-predicate (name stop-condition result-if-stopped default-result)
  `(defun ,name (a b)
     (block check
       (dolist (k (js-set-order a))
         (when ,stop-condition
           (return-from check ,result-if-stopped)))
       ,default-result)))

;;; (define-js-set-filter-op name docstring condition)  — CONDITION may reference A, B, K.
(defmacro define-js-set-filter-op (name docstring condition)
  `(defun ,name (a b)
     ,docstring
     (let ((result (%js-make-set)))
       (dolist (k (js-set-order a))
         (when ,condition (%js-set-add result k)))
       result)))

(defun %js-set-union (a b)
  "New set: union of A and B (insertion order: A first, then new elements from B)."
  (let ((result (%js-make-set)))
    (%js-set-copy-all result a)
    (dolist (k (%js-set-like-keys b))
      (%js-set-add result k))
    result))

(define-js-set-filter-op %js-set-intersection
  "New set: elements in both A and B (order from A)."
  (%js-set-like-has b k))

(define-js-set-filter-op %js-set-difference
  "New set: elements in A but not in B."
  (not (%js-set-like-has b k)))

(defun %js-set-symmetric-difference (a b)
  "New set: elements in A or B but not both."
  (let ((result (%js-make-set)))
    (dolist (k (js-set-order a))
      (unless (%js-set-like-has b k) (%js-set-add result k)))
    (dolist (k (%js-set-like-keys b))
      (unless (%js-set-has a k) (%js-set-add result k)))
    result))

(define-js-set-predicate %js-set-is-subset-of
  (not (%js-set-like-has b k)) nil t)

(define-js-set-predicate %js-set-is-disjoint-from
  (%js-set-like-has b k) nil t)

;;; isSupersetOf(A, B) means every element of B is in A — must iterate B, not A.
;;; The macro always iterates A, so this is a plain defun instead.
(defun %js-set-is-superset-of (a b)
  (dolist (k (%js-set-like-keys b) t)
    (unless (%js-set-has a k)
      (return nil))))

;;; -----------------------------------------------------------------------
;;;  Iterator helpers (ES2025)
;;; -----------------------------------------------------------------------

;;; Iterators are represented as closures that return (:value v :done nil/t)
;;; or as JS objects with a "next" method.

(defun %js-zip-type-error (message)
  (%js-throw
   (%js-make-error-instance
    *js-type-error-class*
    message)))

(defun %js-zip-primitive-p (value)
  (or (null value)
      (stringp value)
      (numberp value)
      (eq value t)
      (eq value nil)
      (typep value 'js-symbol)
      (typep value 'js-bigint)))

(defun %js-zip-normalize-iterable (value)
  "Normalize VALUE to a JS iterator for Iterator.zip / zipKeyed."
  (cond
    ((%js-zip-primitive-p value)
     (%js-zip-type-error "Iterator.zip expects iterable objects"))
    ((%js-vec-p value)
     (%js-vec-to-iter value))
    ((typep value 'js-map)
     (%js-map-entries value))
    ((typep value 'js-set)
     (%js-set-keys value))
    ((%js-ht-p value)
     (multiple-value-bind (next present-p) (gethash "next" value)
       (when (and present-p (functionp next))
         (return-from %js-zip-normalize-iterable value)))
     (multiple-value-bind (iter-fn iter-present-p) (gethash "@@iterator" value)
       (when (and iter-present-p (functionp iter-fn))
         (return-from %js-zip-normalize-iterable
           (%js-zip-normalize-iterable (%js-funcall iter-fn)))))
     (%js-zip-type-error "Iterator.zip expects iterable objects"))
    (t
     (%js-zip-type-error "Iterator.zip expects iterable objects"))))

(defun %js-zip-normalize-options (options)
  (when (and options (not (hash-table-p options)))
    (%js-zip-type-error "Iterator.zip options must be an object")))

(defun %js-zip-collect-source-iterators (iterables)
  (let ((outer (%js-zip-normalize-iterable iterables))
        (sources nil))
    (loop
      (multiple-value-bind (item done) (%js-iter-next outer)
        (when done
          (return (nreverse sources)))
        (push (%js-zip-normalize-iterable item) sources)))))

(defun %js-zip-collect-padding (padding-option iter-count)
  (cond
    ((null padding-option)
      (loop repeat iter-count collect +js-undefined+))
    ((%js-zip-primitive-p padding-option)
     (%js-zip-type-error "Iterator.zip padding must be iterable"))
    (t
     (let ((padding-iter (%js-zip-normalize-iterable padding-option))
           (padding nil)
           (count 0))
       (block padding-limit
         (loop
           (multiple-value-bind (item done) (%js-iter-next padding-iter)
             (when done
               (return-from padding-limit))
             (push item padding)
             (incf count)
             (when (>= count iter-count)
               (return-from padding-limit)))))
       (let ((result (nreverse padding)))
         (loop repeat (max 0 (- iter-count count))
               do (setf result (append result (list +js-undefined+))))
         result)))))

(defun %js-zip-collect-keyed-padding (padding-option keys)
  (cond
    ((null padding-option)
     (loop for _ in keys collect +js-undefined+))
    ((not (hash-table-p padding-option))
     (%js-zip-type-error "Iterator.zip padding must be an object"))
    (t
     (loop for key in keys
           collect (multiple-value-bind (value present-p) (gethash key padding-option)
                     (if present-p value +js-undefined+))))))

(defun %js-zip-strict-remaining-exhausted-p (sources)
  (dolist (iter sources t)
    (when iter
      (multiple-value-bind (value done) (%js-iter-next iter)
        (declare (ignore value))
        (unless done
          (return-from %js-zip-strict-remaining-exhausted-p nil))))))

(defun %js-zip-read-mode (options)
  (let ((mode "shortest")
        (padding-option nil))
    (when options
      (multiple-value-bind (value present-p) (gethash "mode" options)
        (when present-p
          (setf mode value))))
    (unless (member mode '("shortest" "longest" "strict") :test #'string=)
      (%js-zip-type-error "Iterator.zip mode must be shortest, longest, or strict"))
    (when (string= mode "longest")
      (when options
        (multiple-value-bind (value present-p) (gethash "padding" options)
          (when present-p
            (setf padding-option value)))))
    (values mode padding-option)))

(defun %js-iterator-zip (iterables &optional options)
  "Iterator.zip(iterables[, options]) → iterator of arrays."
  (%js-zip-normalize-options options)
  (multiple-value-bind (mode padding-option) (%js-zip-read-mode options)
    (let* ((sources (%js-zip-collect-source-iterators iterables))
           (padding (if (string= mode "longest")
                        (%js-zip-collect-padding padding-option (length sources))
                        (loop repeat (length sources) collect +js-undefined+)))
           (done-p nil))
      (%js-make-cl-iterator
       (lambda ()
         (block step
           (when done-p
             (return-from step :done))
           (when (null sources)
             (setf done-p t)
             (return-from step :done))
           (let ((results nil)
                 (any-done-p nil)
                 (all-done-p t))
             (loop for idx from 0
                   for iter in sources
                   for pad in padding
                   do (cond
                        ((null iter)
                         (push pad results))
                        (t
                         (multiple-value-bind (next-value exhausted-p) (%js-iter-next iter)
                           (if exhausted-p
                             (progn
                                 (setf any-done-p t)
                                 (setf (nth idx sources) nil)
                                 (push pad results))
                               (progn
                                 (setf all-done-p nil)
                                 (push next-value results)))))))
             (when any-done-p
               (cond
                 ((and (string= mode "strict") (not all-done-p))
                  (%js-zip-type-error "Iterator.zip strict mode requires equal-length iterables"))
                 ((not (string= mode "longest"))
                  (setf done-p t)
                  (return-from step :done))
                 (all-done-p
                  (setf done-p t)
                  (return-from step :done))))
             (when all-done-p
               (setf done-p t)
               (return-from step :done))
             (cons (apply #'%js-make-array (nreverse results)) nil))))))))

(defun %js-iterator-zip-keyed (iterables &optional options)
  "Iterator.zipKeyed(iterables[, options]) → iterator of null-prototype objects."
  (%js-zip-normalize-options options)
  (multiple-value-bind (mode padding-option) (%js-zip-read-mode options)
    (unless (hash-table-p iterables)
      (%js-zip-type-error "Iterator.zipKeyed expects an object"))
    (let* ((keys nil)
           (sources nil)
           (all-keys (%js-object-own-string-property-keys iterables))
           (padding (if (string= mode "longest")
                        nil
                        (loop repeat (length all-keys) collect +js-undefined+)))
           (done-p nil))
        (loop for key across all-keys do
          (multiple-value-bind (value present-p) (gethash key iterables)
            (when present-p
              (push key keys)
              (push (%js-zip-normalize-iterable value) sources))))
      (setf keys (nreverse keys)
            sources (nreverse sources))
      (when (string= mode "longest")
        (setf padding (%js-zip-collect-keyed-padding padding-option keys)))
      (%js-make-cl-iterator
       (lambda ()
         (block step
           (when done-p
             (return-from step :done))
           (when (null sources)
             (setf done-p t)
             (return-from step :done))
           (let ((results (%js-object-create +js-null+))
                 (any-done-p nil)
                 (all-done-p t))
             (loop for idx from 0
                   for key in keys
                   for iter in sources
                   for pad in padding
                   do (cond
                        ((null iter)
                         (setf (gethash key results) pad))
                        (t
                         (multiple-value-bind (next-value exhausted-p) (%js-iter-next iter)
                           (if exhausted-p
                             (progn
                                 (setf any-done-p t)
                                 (setf (nth idx sources) nil)
                                 (setf (gethash key results) pad))
                               (progn
                                 (setf all-done-p nil)
                                 (setf (gethash key results) next-value)))))))
             (when any-done-p
               (cond
                 ((and (string= mode "strict") (not all-done-p))
                  (%js-zip-type-error "Iterator.zip strict mode requires equal-length iterables"))
                 ((not (string= mode "longest"))
                  (setf done-p t)
                  (return-from step :done))
                 (all-done-p
                  (setf done-p t)
                  (return-from step :done))))
             (when all-done-p
               (setf done-p t)
               (return-from step :done))
             (cons results nil))))))))

(defun %js-iter-next (iter)
  "Advance iter; return (values value done-p)."
  (let* ((result (if (functionp iter)
                     (funcall iter)
                     (funcall (gethash "next" iter))))
         (ht-p  (%js-ht-p result)))
    (values (if ht-p (gethash "value" result) result)
            (if ht-p (%js-truthy (gethash "done" result)) nil))))

(defun %js-make-cl-iterator (get-next-fn)
  "Create a JS iterator object from a CL thunk that returns (value . done)."
  (let ((ht (%js-make-ht)))
    (setf (gethash "next" ht)
          (lambda ()
            (let ((pair (funcall get-next-fn)))
              (if (eq pair :done)
                  (%js-make-object "value" +js-undefined+ "done" t)
                  (%js-make-object "value" (car pair) "done" nil)))))
    ;; Attach ES2025 Iterator.prototype helpers and @@iterator
    (%js-add-iterator-helpers! ht)))

(defun %js-vec-to-iter (vec)
  "Create iterator over a vector."
  (let ((i 0))
    (%js-make-cl-iterator
     (lambda ()
       (if (>= i (length vec))
           :done
           (let ((v (aref vec i)))
             (incf i)
             (cons v nil)))))))

;;; ─── Stateless transformers (no extra mutable state needed) ──────────────────

(defun %js-iterator-map (iter fn)
  (%js-make-cl-iterator
   (lambda ()
     (multiple-value-bind (val done) (%js-iter-next iter)
       (if done :done (cons (%js-funcall fn val) nil))))))

(defun %js-iterator-filter (iter fn)
  (%js-make-cl-iterator
   (lambda ()
     (loop
       (multiple-value-bind (val done) (%js-iter-next iter)
         (when done (return :done))
         (when (%js-truthy (%js-funcall fn val))
           (return (cons val nil))))))))

;;; ─── Stateful transformers (carry extra mutable state in closure) ─────────────

(defun %js-iterator-take (iter n)
  (let ((count 0))
    (%js-make-cl-iterator
     (lambda ()
       (if (>= count n)
           :done
           (multiple-value-bind (val done) (%js-iter-next iter)
             (if done :done (progn (incf count) (cons val nil)))))))))

(defun %js-iterator-drop (iter n)
  ;; init-done guards the one-time skip phase so it never re-runs on later calls.
  ;; Using return-from here was unsafe: the outer function's block is already gone
  ;; by the time the stored lambda is invoked, which is undefined behavior in CL.
  (let ((init-done nil))
    (%js-make-cl-iterator
     (lambda ()
       (unless init-done
         (setf init-done t)
         (dotimes (_ n)
           (multiple-value-bind (v d) (%js-iter-next iter)
             (declare (ignore v))
             (when d (return)))))
       (multiple-value-bind (val done) (%js-iter-next iter)
         (if done :done (cons val nil)))))))

(defun %js-iterator-flat-map (iter fn)
  (let ((inner nil))
    (%js-make-cl-iterator
     (lambda ()
       (loop
         (when inner
           (multiple-value-bind (val done) (%js-iter-next inner)
             (unless done (return (cons val nil)))
             (setf inner nil)))
         (multiple-value-bind (val done) (%js-iter-next iter)
           (when done (return :done))
           (let ((mapped (%js-funcall fn val)))
             (setf inner (if (%js-vec-p mapped)
                             (%js-vec-to-iter mapped)
                             mapped)))))))))

;;; ─── Terminal consumers (return a single value, not an iterator) ──────────────

(defmacro %js-doiter ((var iter &optional (done-result '+js-undefined+)) &body body)
  "Iterate JS iterator ITER, binding VAR to each successive value.
BODY runs for each element; DONE-RESULT is returned when the iterator exhausts."
  (let ((done (gensym "done")))
    `(loop
       (multiple-value-bind (,var ,done) (%js-iter-next ,iter)
         (when ,done (return ,done-result))
         ,@body))))

(defun %js-iterator-reduce (iter fn &optional (init +js-undefined+))
  (let ((acc init) (first-p (eq init +js-undefined+)))
    (%js-doiter (val iter acc)
      (if first-p
          (setf acc val first-p nil)
          (setf acc (%js-funcall fn acc val))))))

(defun %js-iterator-to-array (iter)
  (let ((result (make-array 0 :element-type t :adjustable t :fill-pointer 0)))
    (%js-doiter (val iter result)
      (vector-push-extend val result))))

(defun %js-iterator-for-each (iter fn)
  (%js-doiter (val iter +js-undefined+)
    (%js-funcall fn val)))

(defun %js-iterator-some (iter fn)
  (%js-doiter (val iter nil)
    (when (%js-truthy (%js-funcall fn val)) (return t))))

(defun %js-iterator-every (iter fn)
  (%js-doiter (val iter t)
    (unless (%js-truthy (%js-funcall fn val)) (return nil))))

(defun %js-iterator-find (iter fn)
  (%js-doiter (val iter +js-undefined+)
    (when (%js-truthy (%js-funcall fn val)) (return val))))

(defun %js-iterator-concat (&rest items)
  "Iterator.concat(...items): lazily chain normalized iterables in order."
  (let ((sources (mapcar #'%js-iterator-from-iterable items))
        (current nil))
    (%js-make-cl-iterator
     (lambda ()
       (loop
         (when current
           (multiple-value-bind (val done) (%js-iter-next current)
             (unless done (return (cons val nil)))
             (setf current nil)))
         (when (null sources)
           (return :done))
         (setf current (pop sources)))))))

;;; ─── ES2025 Iterator.prototype helpers ────────────────────────────────────────

;;; Data table: ES2025 Iterator.prototype method names → CL implementation symbols.
;;; Each function takes the iterator itself as its first argument, followed by any
;;; additional parameters — so the binding loop can use (apply impl self args)
;;; uniformly across all methods.
(defparameter *%js-iterator-method-names*
  '(("map"     . %js-iterator-map)
    ("filter"  . %js-iterator-filter)
    ("take"    . %js-iterator-take)
    ("drop"    . %js-iterator-drop)
    ("flatMap" . %js-iterator-flat-map)
    ("reduce"  . %js-iterator-reduce)
    ("toArray" . %js-iterator-to-array)
    ("forEach" . %js-iterator-for-each)
    ("some"    . %js-iterator-some)
    ("every"   . %js-iterator-every)
    ("find"    . %js-iterator-find))
  "ES2025 Iterator.prototype methods: JS name -> CL function (iter &rest args).")

(defun %js-add-iterator-helpers! (it)
  "Attach ES2025 Iterator.prototype methods and @@iterator to IT.
Binding logic is uniform: each method dispatches through *%js-iterator-method-names*."
  (setf (gethash "@@iterator" it) (lambda () it))
  (dolist (entry *%js-iterator-method-names*)
    (let ((key (car entry))
          (fn  (symbol-function (cdr entry))))
      (setf (gethash key it)
            (let ((impl fn) (self it))
              (lambda (&rest args) (apply impl self args))))))
  it)
