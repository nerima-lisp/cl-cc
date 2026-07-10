;;;; packages/javascript/src/runtime-builtins-platform-abort.lisp — AbortController / AbortSignal helpers

(in-package :cl-cc/javascript)

(defun %js-abort-reason-object (name message)
  "Create a small DOMException-like abort reason object."
  (%js-make-object "name" name
                   "message" message
                   "toString" (lambda ()
                                (concatenate 'string name ": " message))))

(defun %js-abort-default-reason ()
  (%js-abort-reason-object "AbortError" "This operation was aborted"))

(defun %js-abort-timeout-reason ()
  (%js-abort-reason-object "TimeoutError" "The operation timed out"))

(defun %js-abort-event (target)
  (%js-make-object "type" "abort"
                   "target" target
                   "currentTarget" target
                   "defaultPrevented" nil))

(defun %js-abort-signal-listeners (signal)
  (or (gethash "__abort_listeners__" signal)
      (setf (gethash "__abort_listeners__" signal) nil)))

(defun %js-abort-call-listener (listener event signal)
  (cond
    ((or (funcall *js-callable-p* listener)
         (and (%js-ht-p listener) (gethash "__call__" listener)))
     (%js-call-with-this signal listener (list event)))
    ((and (%js-ht-p listener) (gethash "handleEvent" listener))
     (%js-call-with-this listener (gethash "handleEvent" listener) (list event)))))

(defun %js-abort-signal-dispatch-abort (signal)
  (let ((event (%js-abort-event signal))
        (handler (gethash "onabort" signal)))
    (when (and handler (not (eq handler +js-undefined+)))
      (%js-abort-call-listener handler event signal))
    (dolist (listener (copy-list (%js-abort-signal-listeners signal)))
      (%js-abort-call-listener listener event signal))
    t))

(defun %js-abort-signal-abort (signal reason)
  "Abort SIGNAL once with REASON, dispatching abort listeners synchronously."
  (unless (gethash "aborted" signal)
    (setf (gethash "aborted" signal) t
          (gethash "reason" signal) reason)
    (%js-abort-signal-dispatch-abort signal))
  +js-undefined+)

(defun %js-make-abort-signal (&optional (aborted nil) (reason +js-undefined+))
  "Create an AbortSignal-like EventTarget object."
  (let ((signal (%js-make-object "aborted" aborted
                                 "reason" reason
                                 "onabort" +js-undefined+)))
    (setf (gethash "__abort_listeners__" signal) nil)
    (setf (gethash "addEventListener" signal)
          (lambda (type listener &rest _)
            (declare (ignore _))
            (when (and (string= (%js-to-string type) "abort")
                       listener
                       (not (eq listener +js-undefined+))
                       (not (member listener (%js-abort-signal-listeners signal) :test #'eq)))
              (setf (gethash "__abort_listeners__" signal)
                    (append (%js-abort-signal-listeners signal) (list listener))))
            +js-undefined+))
    (setf (gethash "removeEventListener" signal)
          (lambda (type listener &rest _)
            (declare (ignore _))
            (when (string= (%js-to-string type) "abort")
              (setf (gethash "__abort_listeners__" signal)
                    (remove listener (%js-abort-signal-listeners signal) :test #'eq)))
            +js-undefined+))
    (setf (gethash "dispatchEvent" signal)
          (lambda (event)
            (when (and (%js-ht-p event)
                       (string= (%js-to-string (gethash "type" event)) "abort"))
              (%js-abort-signal-dispatch-abort signal))
            t))
    (setf (gethash "throwIfAborted" signal)
          (lambda ()
            (when (gethash "aborted" signal)
              (%js-throw (gethash "reason" signal)))
            +js-undefined+))
    signal))

(defun %js-make-abort-controller ()
  "Create an AbortController with a real AbortSignal-like signal."
  (let ((signal (%js-make-abort-signal)))
    (%js-make-object "signal" signal
                     "abort" (lambda (&optional (reason +js-undefined+ reason-p))
                               (%js-abort-signal-abort
                                signal
                                (if reason-p reason (%js-abort-default-reason)))))))

(defun %js-make-abort-controller-constructor ()
  "Callable/newable AbortController global."
  (%js-make-object
   "__new__" (lambda (&rest _)
               (declare (ignore _))
               (%js-make-abort-controller))
   "__call__" (lambda (&rest _)
                (declare (ignore _))
                (%js-make-abort-controller))))

(defun %js-abort-signal-aborted (&optional (reason +js-undefined+ reason-p))
  "AbortSignal.abort(reason): return an already-aborted signal."
  (%js-make-abort-signal t (if reason-p reason (%js-abort-default-reason))))

(defun %js-abort-signal-timeout (&optional _)
  "AbortSignal.timeout(ms): synchronous runtime model returns an aborted signal."
  (declare (ignore _))
  (%js-make-abort-signal t (%js-abort-timeout-reason)))

(defun %js-abort-signal-any (signals)
  "AbortSignal.any(iterable): abort when the first input signal aborts."
  (let ((combined (%js-make-abort-signal)))
    (labels ((abort-from (signal)
               (%js-abort-signal-abort
                combined
                (gethash "reason" signal (%js-abort-default-reason)))))
      (%js-for-of signals
        (lambda (signal)
          (when (%js-ht-p signal)
            (if (gethash "aborted" signal)
                (abort-from signal)
                (let ((add-listener (gethash "addEventListener" signal)))
                  (when (functionp add-listener)
                    (%js-call-with-this
                     signal add-listener
                     (list "abort" (lambda (&rest _) (declare (ignore _)) (abort-from signal)))))))))))
    combined))

(defun %js-make-abort-signal-constructor ()
  "AbortSignal global with standard static helpers."
  (%js-make-object
   "abort" #'%js-abort-signal-aborted
   "timeout" #'%js-abort-signal-timeout
   "any" #'%js-abort-signal-any
   "__call__" (lambda (&rest _)
                (declare (ignore _))
                (%js-make-abort-signal))))
