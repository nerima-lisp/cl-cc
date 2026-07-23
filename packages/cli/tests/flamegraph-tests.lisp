(in-package :cl-cc/test)


(it-sequential "cli-flamegraph-reads-perf-map-lines"
  (uiop:with-temporary-file (:pathname input :type "map" :keep t)
    (with-open-file (out input :direction :output :if-exists :supersede)
      (write-line "1000 20 jit-function" out)
      (write-line "1020 10 normal-function" out))
    (let ((samples (cl-cc/cli::%read-flamegraph-samples-from-file input)))
      (expect (= #x20 (gethash "jit;jit-function" samples)) :to-be-truthy)
      (expect (= #x10 (gethash "jit;normal-function" samples)) :to-be-truthy))
    (ignore-errors (delete-file input))))

(it-sequential "cli-flamegraph-svg-contains-rects-and-colors"
  (uiop:with-temporary-file (:pathname output :type "svg" :keep t)
    (let ((samples (make-hash-table :test #'equal)))
      (setf (gethash "cpu;hot-function" samples) 4)
      (setf (gethash "gc;minor-gc" samples) 2)
      (setf (gethash "jit;jit-compile" samples) 3)
      (cl-cc/cli::%write-flamegraph-svg output samples)
      (let ((svg (cl-cc/cli::%read-file output)))
        (expect (search "<svg" svg) :to-be-truthy)
        (expect (search "<rect" svg) :to-be-truthy)
        (expect (search "rgb(90,140,255)" svg) :to-be-truthy)
        (expect (search "rgb(255,165,0)" svg) :to-be-truthy)))
    (ignore-errors (delete-file output))))
