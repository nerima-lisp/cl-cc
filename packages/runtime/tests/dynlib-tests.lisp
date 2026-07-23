;;;; packages/runtime/tests/dynlib-tests.lisp — FR-719 dynamic library tests

(in-package :cl-cc/test)

;;; cl-cc/ffi is provided at load-time by the cl-cc umbrella system.
;;; At compile-time in the test build, the package may not exist yet.
;;; Create a placeholder so the reader can intern symbols.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-cc/ffi)
    (defpackage :cl-cc/ffi
      (:use :cl)
      (:export #:dl-lib #:dl-lib-p #:dl-lib-name #:dl-lib-handle #:dl-lib-loaded
               #:load-shared-library #:load-framework #:unload-shared-library
               #:list-loaded-libraries #:find-foreign-symbol))))


(it-sequential "fr-417-pinned-unboxed-array-provides-data-pointer"
  (let* ((array (make-array 4 :element-type '(unsigned-byte 8)
                            :initial-contents '(1 2 3 4)))
         (buffer (cl-cc/runtime:rt-pin-unboxed-array array)))
    (expect (cl-cc/runtime:rt-pinned-unboxed-array-buffer-p buffer) :to-be-truthy)
    (expect (cl-cc/runtime:rt-pinned-unboxed-array-buffer-array buffer) :to-be array)
    (expect (= 4 (cl-cc/runtime:rt-pinned-unboxed-array-buffer-length buffer)) :to-be-truthy)
    (expect (cl-cc/runtime:rt-pinned-array-data-pointer buffer) :to-be-truthy)
    (cl-cc/runtime:rt-release-pinned-array buffer)
    (expect (cl-cc/runtime:rt-pinned-unboxed-array-buffer-released-p buffer) :to-be-truthy)
    (signals error (cl-cc/runtime:rt-pinned-array-data-pointer buffer))))

(defun %test-libc-path ()
  #+darwin "/usr/lib/libSystem.B.dylib"
  #+linux "libc.so.6"
  #-(or darwin linux) nil)

(it-sequential "dynlib-load-find-unload-libc-symbol"
  (let ((path (%test-libc-path)))
    (when path
      (let ((library (cl-cc/ffi:load-shared-library path)))
        (unwind-protect
             (progn
               (expect (cl-cc/ffi:dl-lib-p library) :to-be-truthy)
               (expect (cl-cc/ffi:dl-lib-name library) :to-equal path)
               (expect (cl-cc/ffi:dl-lib-loaded library) :to-be-truthy)
               (expect (functionp (cl-cc/ffi:find-foreign-symbol "printf" library)) :to-be-truthy)
               (expect (cl-cc/ffi:find-foreign-symbol "cl_cc_symbol_that_does_not_exist" library) :to-be-null))
          (cl-cc/ffi:unload-shared-library library))
        (expect (cl-cc/ffi:dl-lib-loaded library) :to-be-falsy)))))

(it-sequential "dynlib-found-symbol-is-callable"
  (let ((path (%test-libc-path)))
    (when path
      (let ((library (cl-cc/ffi:load-shared-library path)))
        (unwind-protect
             (let ((getpid (cl-cc/ffi:find-foreign-symbol "getpid" library)))
               (expect (functionp getpid) :to-be-truthy)
               (expect (integerp (funcall getpid)) :to-be-truthy))
          (cl-cc/ffi:unload-shared-library library))))))

#+darwin
(it-sequential "dynlib-load-framework-foundation"
  (let ((library (cl-cc/ffi:load-framework "Foundation")))
    (unwind-protect
         (progn
            (expect (cl-cc/ffi:dl-lib-p library) :to-be-truthy)
            (expect (cl-cc/ffi:dl-lib-name library) :to-equal "/System/Library/Frameworks/Foundation.framework/Foundation"))
      (cl-cc/ffi:unload-shared-library library))))
