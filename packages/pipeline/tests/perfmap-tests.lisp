(in-package :cl-cc/test)


(it-sequential "pipeline-perf-map-line-validates-format"
  (expect (cl-cc/pipeline:perf-map-line-valid-p "1000 2A FOO") :to-be-truthy)
  (expect (cl-cc/pipeline:perf-map-line-valid-p "1000 nope FOO") :to-be-falsy)
  (expect (cl-cc/pipeline:perf-map-line-valid-p "1000 2A") :to-be-falsy))

(it-sequential "pipeline-write-perf-map-entry-emits-hex-fields"
  (let ((stream (make-string-output-stream)))
    (cl-cc/pipeline:write-perf-map-entry stream #x1000 #x2a 'sample-function)
    (let ((line (string-trim '(#\Newline #\Return)
                             (get-output-stream-string stream))))
      (expect line :to-equal "1000 2A SAMPLE-FUNCTION")
      (expect (cl-cc/pipeline:perf-map-line-valid-p line) :to-be-truthy))))

(it-sequential "pipeline-write-perf-map-for-native-code-appends-map-line"
  (uiop:with-temporary-file (:pathname path :type "map" :keep t)
    (let ((program (cl-cc:make-vm-program
                    :instructions (list (cl-cc:make-vm-const :dst :r0 :value 1)
                                        (cl-cc:make-vm-halt :reg :r0))))
          (bytes #(1 2 3 4)))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (let ((cl-cc/pipeline:*perf-map-stream* out))
          (cl-cc/pipeline:write-perf-map-for-native-code program bytes :output-file "unit-main")))
      (let ((line (with-open-file (in path :direction :input) (read-line in nil nil))))
        (expect (cl-cc/pipeline:perf-map-line-valid-p line) :to-be-truthy)
        (expect (search "unit-main" line :test #'char-equal) :to-be-truthy)))
    (ignore-errors (delete-file path))))
