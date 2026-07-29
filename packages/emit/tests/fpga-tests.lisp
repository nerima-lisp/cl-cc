(in-package :cl-cc/test)

(it-sequential
  "fpga-hls-builds-a-timed-resource-shared-pipeline"
  (let* ((program
           (cl-cc/emit:lower-fpga-hls
             (quote pipeline)
             (quote (lambda (x y) (+ (* x y) (* x x))))
             :resource-limits (quote ((* . 1)))
             :initiation-interval 4
             :clock-period-ns 3.0))
         (schedule (cl-cc/emit:fpga-hls-program-schedule program))
         (multiply-entries
           (remove-if-not
             (lambda (entry) (eq (getf entry :operator) (quote *)))
             schedule))
         (addition-entry (find (quote +) schedule :key (lambda (entry) (getf entry :operator))))
         (verilog (cl-cc/emit:emit-fpga-verilog program))
         (vhdl (cl-cc/emit:emit-fpga-vhdl program)))
    (expect (mapcar (lambda (entry) (getf entry :stage)) multiply-entries)
            :to-equal
            (quote (0 1)))
    (expect (every (lambda (entry) (zerop (getf entry :resource))) multiply-entries)
            :to-be-truthy)
    (expect (getf addition-entry :stage) :to-equal 2)
    (expect (getf addition-entry :lhs) :to-equal (quote (:value 0)))
    (expect (getf addition-entry :rhs) :to-equal (quote (:value 1)))
    (expect (search "assign mul_fu_0_result = (mul_fu_0_lhs * mul_fu_0_rhs);" verilog)
            :to-be-truthy)
    (expect (search "mul_fu_1" verilog) :to-be nil)
    (expect (search "integer ii_counter = 0" verilog) :to-be-truthy)
    (expect (search "output result_valid;" verilog) :to-be-truthy)
    (expect (search "assign result_valid = result_valid_reg;" verilog) :to-be-truthy)
    (expect (search "ii_counter <= 3" verilog) :to-be-truthy)
    (expect (search "x_stage_0 <= x" verilog) :to-be-truthy)
    (expect (search "x_stage_1 <= x_stage_0" verilog) :to-be-truthy)
    (expect (search "pipeline_valid <= pipeline_valid << 1" verilog) :to-be-truthy)
    (expect (search "pipeline_valid[1] ? x_stage_1" verilog) :to-be-truthy)
    (expect (search "pipeline_busy" verilog) :to-be nil)
    (expect (search "op_0_reg <= mul_fu_0_result;" verilog) :to-be-truthy)
    (expect (search "op_1_reg <= mul_fu_0_result;" verilog) :to-be-truthy)
    (expect (search "mul_fu_0_result <=" vhdl) :to-be-truthy)
    (expect (search "mul_fu_1" vhdl) :to-be nil)
    (expect (search "signal ii_counter : integer range 0 to 3" vhdl) :to-be-truthy)
    (expect (search "result_valid : out std_logic" vhdl) :to-be-truthy)
    (expect (search "result_valid <= result_valid_reg;" vhdl) :to-be-truthy)
    (expect (search "x_stage_1 <= x_stage_0" vhdl) :to-be-truthy)
    (expect (search "pipeline_valid(1) <= pipeline_valid(0)" vhdl) :to-be-truthy)
    (expect (search "pipeline_busy" vhdl) :to-be nil)
    (expect (search "op_2_reg <= add_fu_0_result;" vhdl) :to-be-truthy)))

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
         (schedule (cl-cc/emit:fpga-hls-program-schedule program))
         (fsm-entry (car (last schedule)))
         (verilog (cl-cc/emit:emit-fpga-verilog program))
         (vhdl (cl-cc/emit:emit-fpga-vhdl program)))
    (expect (car ir) :to-equal :fsm)
    (expect (getf fsm-entry :kind) :to-equal :fsm)
    (expect (getf fsm-entry :selector) :to-equal (quote (:input opcode "opcode")))
    (expect (length (getf fsm-entry :states)) :to-equal 2)
    (expect (getf fsm-entry :default) :to-equal (quote (:constant 255)))
    (expect (search "opcode_stage_1 == 0" verilog) :to-be-truthy)
    (expect (search "opcode_stage_1 == 2" verilog) :to-be-truthy)
    (expect (search "boolean'pos(opcode_stage_1 = 0) * (value_stage_1)" vhdl)
            :to-be-truthy)
    (expect (search "boolean'pos(opcode_stage_1 = 2) * (op_0_reg)" vhdl)
            :to-be-truthy)
    (expect (search " when " vhdl) :to-be nil)))

(it-sequential
  "fpga-hls-supports-pure-let-bindings"
  (let* ((program
           (cl-cc/emit:lower-fpga-hls
             (quote square_sum)
             (quote
               (lambda (x y)
                 (let ((sum (+ x y)))
                   (* sum sum))))))
         (schedule (cl-cc/emit:fpga-hls-program-schedule program))
         (multiply-entry (third schedule))
         (verilog (cl-cc/emit:emit-fpga-verilog program)))
    (expect (length schedule) :to-equal 3)
    (expect (getf multiply-entry :lhs) :to-equal (quote (:value 0)))
    (expect (getf multiply-entry :rhs) :to-equal (quote (:value 1)))
    (expect (search "op_0_reg" verilog) :to-be-truthy)
    (expect (search "op_1_reg" verilog) :to-be-truthy)
    (expect (search "mul_fu_0_result" verilog) :to-be-truthy)))

(it-sequential
  "fpga-hls-let-initializers-use-the-outer-environment"
  (let* ((program
           (cl-cc/emit:lower-fpga-hls
             (quote parallel_let)
             (quote (lambda (x) (let ((x 1) (y x)) (+ x y))))))
         (entry (first (cl-cc/emit:fpga-hls-program-schedule program)))
         (verilog (cl-cc/emit:emit-fpga-verilog program)))
    (expect (getf entry :lhs) :to-equal (quote (:constant 1)))
    (expect (getf entry :rhs) :to-equal (quote (:input x "x")))
    (expect (search "assign add_fu_0_lhs = pipeline_valid[0] ? 1 : 0;" verilog)
            :to-be-truthy)
    (expect (search "assign add_fu_0_rhs = pipeline_valid[0] ? x_stage_0 : 0;" verilog)
            :to-be-truthy)))

(progn (it-sequential
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
      0))) (it-sequential
  "fpga-hls-emits-legal-vhdl-integer-bitwise-operations"
  (let* ((program
           (cl-cc/emit:lower-fpga-hls
             (quote bitwise)
             (quote (lambda (x y) (logxor (logand x y) (logior x y))))))
         (vhdl (cl-cc/emit:emit-fpga-vhdl program)))
    (expect (search "use ieee.numeric_std.all;" vhdl) :to-be-truthy)
    (expect (search "to_integer(to_signed(and_fu_0_lhs, 32) and to_signed(and_fu_0_rhs, 32))" vhdl)
            :to-be-truthy)
    (expect (search "to_integer(to_signed(or_fu_0_lhs, 32) or to_signed(or_fu_0_rhs, 32))" vhdl)
            :to-be-truthy)
    (expect (search "to_integer(to_signed(xor_fu_0_lhs, 32) xor to_signed(xor_fu_0_rhs, 32))" vhdl)
            :to-be-truthy)))
  (it-sequential
    "fpga-hls-accepts-overlapped-transactions-at-ii-one"
    (let* ((program
             (cl-cc/emit:lower-fpga-hls
               (quote overlapped)
               (quote (lambda (x y) (* (+ x y) y)))
               :initiation-interval 1))
           (verilog (cl-cc/emit:emit-fpga-verilog program))
           (vhdl (cl-cc/emit:emit-fpga-vhdl program)))
      (expect (search "ii_counter <= 0" verilog) :to-be-truthy)
      (expect (search "pipeline_valid <= pipeline_valid << 1" verilog) :to-be-truthy)
      (expect (search "pipeline_valid[0] <= 1" verilog) :to-be-truthy)
      (expect (search "result_valid_reg <= pipeline_valid[1]" verilog) :to-be-truthy)
      (expect (search "pipeline_busy" verilog) :to-be nil)
      (expect (search "ii_counter <= 0" vhdl) :to-be-truthy)
      (expect (search "pipeline_valid(0) <= '1'" vhdl) :to-be-truthy)
      (expect (search "result_valid_reg <= pipeline_valid(1)" vhdl) :to-be-truthy)
      (expect (search "pipeline_busy" vhdl) :to-be nil)))
  (it-sequential
    "fpga-hls-rejects-resource-sharing-that-cannot-meet-the-ii"
    (signals
      cl-cc/emit:fpga-hls-error
      (cl-cc/emit:lower-fpga-hls
        (quote multiplier_collision)
        (quote (lambda (x y) (+ (* x y) (* x x))))
        :resource-limits (quote ((* . 1)))
        :initiation-interval 1)))
  (it-sequential
    "fpga-hls-emits-legal-vhdl-comparisons"
    (let* ((program
             (cl-cc/emit:lower-fpga-hls
               (quote comparison)
               (quote (lambda (x y) (< x y)))))
           (vhdl (cl-cc/emit:emit-fpga-vhdl program)))
      (expect (search "boolean'pos(lt_fu_0_lhs < lt_fu_0_rhs)" vhdl)
              :to-be-truthy)
      (expect (search " when " vhdl) :to-be nil))))

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
