;;;; packages/javascript/src/runtime-builtins-object.lisp — JS Object/Reflect/Proxy helpers
;;;;
;;;; These helpers are referenced by runtime-builtins-table.lisp and must be
;;;; loaded before it.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Reflect / Object property-descriptor helpers (for runtime-builtins-table.lisp)
;;; -----------------------------------------------------------------------

(defun %js-reflect-get (target key &optional receiver)
  (if (%js-proxy-object-p target)
      (%js-proxy-get target key (or receiver target))
      (%js-get-prop target key)))

(defun %js-reflect-set (target key value &optional receiver)
  (if (%js-proxy-object-p target)
      (%js-proxy-set target key value (or receiver target))
      (progn
        (%js-set-prop target key value)
        t)))

(defun %js-reflect-has (target key)
  (%js-in key target))

(defun %js-reflect-delete-property (target key)
  (%js-delete target key))

(defun %js-reflect-apply (fn this-arg args)
  (%js-call-with-this this-arg fn (coerce args 'list)))

(defun %js-reflect-construct (target args &optional new-target)
  (declare (ignore new-target))
  (%js-new target (coerce args 'list)))

(defun %js-object-descriptor-slot-key (prefix key)
  (concatenate 'string prefix (%js-to-string key)))

(defun %js-object-own-property-or-accessor-present-p (obj key)
  (let* ((k (%js-to-property-key key))
         (getter-key (%js-object-descriptor-slot-key "__get_" key))
         (setter-key (%js-object-descriptor-slot-key "__set_" key)))
    (or (nth-value 1 (gethash k obj))
        (nth-value 1 (gethash getter-key obj))
        (nth-value 1 (gethash setter-key obj)))))

(defun %js-object-define-property-allowed-p (obj key descriptor)
  (and (%js-ht-p obj)
       (%js-ht-p descriptor)
       (not (%js-object-frozen-p obj))
       (or (%js-object-own-property-or-accessor-present-p obj key)
           (%js-object-extensible-p obj))))

(defun %js-object-apply-property-descriptor (obj key descriptor)
  (when (%js-object-define-property-allowed-p obj key descriptor)
    (let* ((k (%js-to-property-key key))
           (getter-key (%js-object-descriptor-slot-key "__get_" key))
           (setter-key (%js-object-descriptor-slot-key "__set_" key)))
      (multiple-value-bind (val has-value-p) (gethash "value" descriptor)
        (when has-value-p
          (setf (gethash k obj) val)))
      (multiple-value-bind (getter has-getter-p) (gethash "get" descriptor)
        (when has-getter-p
          (setf (gethash getter-key obj) getter)))
      (multiple-value-bind (setter has-setter-p) (gethash "set" descriptor)
        (when has-setter-p
          (setf (gethash setter-key obj) setter))))
    t))

(defun %js-object-data-property-descriptor (obj value)
  (%js-make-object "value" value
                   "writable" (not (%js-object-frozen-p obj))
                   "enumerable" t
                   "configurable" (not (%js-object-sealed-p obj))))

(defun %js-object-accessor-property-descriptor (obj key)
  (let* ((descriptor (%js-make-object "enumerable" t
                                      "configurable" (not (%js-object-sealed-p obj))))
         (getter-key (%js-object-descriptor-slot-key "__get_" key))
         (setter-key (%js-object-descriptor-slot-key "__set_" key)))
    (multiple-value-bind (getter has-getter-p) (gethash getter-key obj)
      (when has-getter-p
        (setf (gethash "get" descriptor) getter)))
    (multiple-value-bind (setter has-setter-p) (gethash setter-key obj)
      (when has-setter-p
        (setf (gethash "set" descriptor) setter)))
    descriptor))

(defun %js-reflect-define-property (target key descriptor)
  (cond
    ((%js-proxy-object-p target)
     (%js-proxy-define-property target key descriptor))
    (t
     (%js-object-apply-property-descriptor target key descriptor))))

(defun %js-reflect-get-own-property-descriptor (target key)
  (%js-object-get-own-property-descriptor target key))

(defun %js-reflect-set-prototype-of (target proto)
  (if (%js-ht-p target)
      (let ((before (%js-object-get-prototype-of target)))
        (%js-object-set-prototype-of target proto)
        (or (eq before proto)
            (eq (%js-object-get-prototype-of target) proto)))
      nil))

(defun %js-reflect-prevent-extensions (target)
  (if (%js-ht-p target)
      (progn
        (%js-object-prevent-extensions target)
        t)
      nil))

(defun %js-object-define-property (obj key descriptor)
  (unless (%js-reflect-define-property obj key descriptor)
    (error "JS TypeError: Cannot define property ~A" (%js-to-string key)))
  obj)

(defun %js-object-define-properties (obj props)
  (when (and (%js-ht-p obj) (%js-ht-p props))
    (maphash (lambda (k v)
               (when (%js-ht-p v)
                 (%js-object-define-property obj k v)))
             props))
  obj)

(defun %js-object-get-own-property-descriptor (obj key)
  (cond
    ((%js-proxy-object-p obj)
     (%js-proxy-get-own-property-descriptor obj key))
    ((%js-ht-p obj)
     (let ((k (%js-to-property-key key)))
       (multiple-value-bind (val found) (gethash k obj)
         (cond
           (found
            (%js-object-data-property-descriptor obj val))
           ((%js-object-own-property-or-accessor-present-p obj k)
            (%js-object-accessor-property-descriptor obj k))
           (t +js-undefined+)))))
    (t +js-undefined+)))

(defun %js-object-get-own-property-descriptors (obj)
  (let ((result (%js-make-ht)))
    (cond
      ((%js-proxy-object-p obj)
       (dolist (key (%js-proxy-key-list (%js-proxy-own-keys obj)))
         (let ((descriptor (%js-object-get-own-property-descriptor obj key)))
           (unless (eq descriptor +js-undefined+)
             (setf (gethash (%js-to-string key) result) descriptor)))))
      ((%js-ht-p obj)
       (maphash (lambda (k v)
                  (unless (%js-internal-key-p k)
                    (setf (gethash k result)
                          (%js-object-data-property-descriptor obj v))))
                obj)
       (maphash (lambda (k v)
                  (declare (ignore v))
                  (let ((property-name (%js-object-accessor-property-name k)))
                    (when (and property-name
                               (not (nth-value 1 (gethash property-name result))))
                      (setf (gethash property-name result)
                            (%js-object-accessor-property-descriptor obj property-name)))))
                obj)))
    result))

(defun %js-proxy-object-p (obj)
  "True when OBJ is a runtime Proxy wrapper."
  (and (%js-ht-p obj)
       (nth-value 1 (gethash "__proxy-target__" obj))
       (nth-value 1 (gethash "__proxy-handler__" obj))))

(defun %js-proxy-target (proxy)
  (gethash "__proxy-target__" proxy))

(defun %js-proxy-handler (proxy)
  (gethash "__proxy-handler__" proxy))

(defun %js-proxy-trap (proxy name)
  (let ((handler (%js-proxy-handler proxy)))
    (when (%js-ht-p handler)
      (multiple-value-bind (trap found) (gethash name handler)
        (when (and found (funcall *js-callable-p* trap))
          trap)))))

(defun %js-proxy-call-trap (proxy name &rest args)
  (let ((trap (%js-proxy-trap proxy name)))
    (if trap
        (values (apply #'%js-funcall trap args) t)
        (values +js-undefined+ nil))))

(defun %js-proxy-key-list (keys)
  (cond
    ((%js-vec-p keys) (coerce keys 'list))
    ((listp keys) keys)
    (t nil)))

(defun %js-proxy-get (proxy key &optional receiver)
  (let ((target (%js-proxy-target proxy)))
    (multiple-value-bind (result trapped)
        (%js-proxy-call-trap proxy "get" target (%js-to-string key) (or receiver proxy))
      (if trapped
          result
          (%js-get-prop target key)))))

(defun %js-proxy-set (proxy key value &optional receiver)
  (let ((target (%js-proxy-target proxy)))
    (multiple-value-bind (result trapped)
        (%js-proxy-call-trap proxy "set" target (%js-to-string key) value (or receiver proxy))
      (if trapped
          (%js-truthy result)
          (progn
            (%js-set-prop target key value)
            t)))))

(defun %js-proxy-has (proxy key)
  (let ((target (%js-proxy-target proxy)))
    (multiple-value-bind (result trapped)
        (%js-proxy-call-trap proxy "has" target (%js-to-string key))
      (if trapped
          (%js-truthy result)
          (%js-in key target)))))

(defun %js-proxy-delete-property (proxy key)
  (let ((target (%js-proxy-target proxy)))
    (multiple-value-bind (result trapped)
        (%js-proxy-call-trap proxy "deleteProperty" target (%js-to-string key))
      (if trapped
          (%js-truthy result)
          (%js-delete target key)))))

(defun %js-proxy-own-keys (proxy)
  (let ((target (%js-proxy-target proxy)))
    (multiple-value-bind (result trapped)
        (%js-proxy-call-trap proxy "ownKeys" target)
      (if trapped
          result
          (%js-object-own-keys target)))))

(defun %js-proxy-define-property (proxy key descriptor)
  (let ((target (%js-proxy-target proxy)))
    (multiple-value-bind (result trapped)
        (%js-proxy-call-trap proxy "defineProperty" target (%js-to-string key) descriptor)
      (if trapped
          (%js-truthy result)
          (%js-reflect-define-property target key descriptor)))))

(defun %js-proxy-get-own-property-descriptor (proxy key)
  (let ((target (%js-proxy-target proxy)))
    (multiple-value-bind (result trapped)
        (%js-proxy-call-trap proxy "getOwnPropertyDescriptor" target (%js-to-string key))
      (if trapped
          result
          (%js-object-get-own-property-descriptor target key)))))

(defun %js-make-proxy-object (target handler)
  "Proxy constructor: stores target+handler and routes common traps at runtime."
  (let ((ht (%js-make-ht)))
    (setf (gethash "__proxy-target__" ht) target
          (gethash "__proxy-handler__" ht) handler)
    ht))
