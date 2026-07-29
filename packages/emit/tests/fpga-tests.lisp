(in-package :cl-cc/test)

(it-sequential
  "fpga-hls-builds-a-timed-resource-shared-pipeline"
  (let* ((program
           (cl-cc/emit:lower-fpga-hls
             (quote pipeline)
             (quote (lambda (x y) (+ (* x y) (* x x))))
             :resource-limits (quote ((* . 1)))
             :clock-period-ns 3.0))
         (schedule (cl-cc/emit:fpga-hls-program-schedule program))
         (multiply-entries (remove-if-not
                             (lambda (entry) (eq (getf entry :operator) (quote *)))
                             schedule))
         (verilog (cl-cc/emit:emit-fpga-verilog program))
         (vhdl (cl-cc/emit:emit-fpga-vhdl program)))
    (expect (> (reduce (function max) schedule
                       :key (lambda (entry) (getf entry :stage))) 0)
            :to-be-truthy)
    (expect (every (lambda (entry) (<= (getf entry :finish-ns) 3.0)) schedule)
            :to-be-truthy)
    (expect (every (lambda (entry) (zerop (getf entry :resource))) multiply-entries)
            :to-be-truthy)
    (expect (search "always @(posedge clk)" verilog) :to-be-truthy)
    (expect (search "pipeline_stage_1" verilog) :to-be-truthy)
    (expect (search "entity pipeline is" vhdl) :to-be-truthy)
    (expect (search "rising_edge(clk)" vhdl) :to-be-truthy)))

(it-sequential
  "fpga-hls-lowers-case-to-fsm-ir"
  (let* ((program
        (cl-cc/emit:lower-fpga-hls
          (quote decode)
          (quote
            (lambda (opcode value)
              (case opcode
                (0 value)
                ((1 2) (+ value 1))
                (otherwise 255))))))
         (ir (cl-cc/emit:fpga-hls-program-ir program))
         (verilog (cl-cc/emit:emit-fpga-verilog program)))
    (expect (car ir) :to-equal :fsm)
    (expect (length (third ir)) :to-equal 2)
    (expect (search "opcode == 0" verilog) :to-be-truthy)
    (expect (search "opcode == 2" verilog) :to-be-truthy)))

(it-sequential
  "fpga-hls-supports-pure-let-bindings"
  (let* ((program
        (cl-cc/emit:lower-fpga-hls
          (quote square-sum)
          (quote
            (lambda (x y)
              (let ((sum (+ x y)))
                (* sum sum))))))
         (verilog (cl-cc/emit:emit-fpga-verilog program)))
    (expect (search "((x + y) * (x + y))" verilog) :to-be-truthy)))

(it-sequential "fpga-hls-let-initializers-use-the-outer-environment" (let* ((program (cl-cc/emit:lower-fpga-hls (quote parallel_let) (quote (lambda (x) (let ((x 1) (y x)) (+ x y)))))) (verilog (cl-cc/emit:emit-fpga-verilog program))) (expect (search "(1 + x)" verilog) :to-be-truthy)))

(it-sequential
  "fpga-hls-rejects-impure-and-unsupported-forms"
  (signals
    cl-cc/emit:fpga-hls-error
    (cl-cc/emit:lower-fpga-hls
      (quote impure)
      (quote
        (lambda (x)
          (setf x (+ x 1))))))
  (signals
    cl-cc/emit:fpga-hls-error
    (cl-cc/emit:lower-fpga-hls
      (quote unknown)
      (quote
        (lambda (x)
          (sin x)))))
  (signals
    cl-cc/emit:fpga-hls-error
    (cl-cc/emit:lower-fpga-hls
      (quote bad-ii)
      (quote
        (lambda (x)
          x))
      :initiation-interval
      0)))

(progn
  (it-sequential
    "fpga-hls-stream-emission-returns-program"
    (let* ((program
             (cl-cc/emit:lower-fpga-hls
               (quote add)
               (quote (lambda (x y) (+ x y)))))
           (verilog-stream (make-string-output-stream))
           (vhdl-stream (make-string-output-stream)))
      (expect (cl-cc/emit:emit-fpga-verilog program verilog-stream) :to-be program)
      (expect (search "module add" (get-output-stream-string verilog-stream)) :to-be-truthy)
      (expect (cl-cc/emit:emit-fpga-vhdl program vhdl-stream) :to-be program)
      (expect (search "entity add is" (get-output-stream-string vhdl-stream)) :to-be-truthy)))
  (it-sequential
    "fpga-hls-rejects-normalized-port-name-collisions"
    (signals
      cl-cc/emit:fpga-hls-error
      (cl-cc/emit:lower-fpga-hls
        (quote duplicate-ports)
        (quote (lambda (x |x|) (+ x |x|)))))))
