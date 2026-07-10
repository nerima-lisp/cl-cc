;;;; packages/javascript/src/runtime-collections-iterators.lisp — iterator helpers

(in-package :cl-cc/javascript)

;;; ─── Iterator primitives ─────────────────────────────────────────────────

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
