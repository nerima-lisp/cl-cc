(in-package :cl-cc/emit)

(define-condition fpga-hls-error (error)
  ((reason :initarg :reason :reader fpga-hls-error-reason))
  (:report
    (lambda (condition stream)
      (format stream "FPGA HLS rejected input: ~A" (fpga-hls-error-reason condition)))))

(defstruct (fpga-hls-program (:constructor %make-fpga-hls-program)) name parameters ir schedule verilog vhdl initiation-interval resource-limits clock-period-ns)

(defparameter *fpga-hls-binary-operators* (quote
    ((+ . "+")
      (- . "-")
      (* . "*")
      (logand . "&")
      (logior . "|")
      (logxor . "^")
      (= . "==")
      (< . "<")
      (<= . "<=")
      (> . ">")
      (>= . ">="))))

(defun %fpga-hls-reject (control &rest arguments)
  (error
    (quote fpga-hls-error)
    :reason
    (apply (function format) nil control arguments)))

(defun %fpga-hls-name (name)
  (let ((text (string-downcase (string name))))
    (unless (and
        (plusp (length text))
        (or (alpha-char-p (char text 0)) (char= (char text 0) #\_))
        (every
          (lambda (character)
            (or (alphanumericp character) (char= character #\_)))
          text))
      (%fpga-hls-reject "invalid Verilog identifier ~S" name))
    text))

(defun %fpga-hls-validate-options (initiation-interval resource-limits clock-period-ns)
  (unless (and (integerp initiation-interval) (plusp initiation-interval))
    (%fpga-hls-reject "initiation interval must be a positive integer"))
  (unless (or (null clock-period-ns) (and (realp clock-period-ns) (plusp clock-period-ns))) (%fpga-hls-reject "clock period must be NIL or a positive real number"))
  (dolist (entry resource-limits)
    (unless (and (consp entry)
                 (assoc (car entry) *fpga-hls-binary-operators*)
                 (integerp (cdr entry))
                 (plusp (cdr entry)))
      (%fpga-hls-reject "invalid resource limit ~S" entry))))



(defun %fpga-hls-lower-expression (form environment)
  (cond
    ((integerp form) (list :constant form))
    ((symbolp form) (or (cdr (assoc form environment)) (%fpga-hls-reject "unbound input ~S" form)))
    ((not (consp form)) (%fpga-hls-reject "unsupported literal ~S" form))
    ((eq (car form) (quote if))
     (unless (= (length form) 4) (%fpga-hls-reject "IF requires three operands"))
     (list :mux (%fpga-hls-lower-expression (second form) environment)
           (%fpga-hls-lower-expression (third form) environment)
           (%fpga-hls-lower-expression (fourth form) environment)))
    ((eq (car form) (quote case))
     (unless (>= (length form) 3) (%fpga-hls-reject "CASE requires clauses"))
     (let ((selector (%fpga-hls-lower-expression (second form) environment)) (states nil) (default nil))
       (dolist (clause (cddr form))
         (unless (and (consp clause) (= (length clause) 2)) (%fpga-hls-reject "CASE clauses require one value expression"))
         (if (member (car clause) (quote (otherwise t)))
             (progn (when default (%fpga-hls-reject "duplicate CASE default"))
                    (setf default (%fpga-hls-lower-expression (second clause) environment)))
             (let ((keys (if (listp (car clause)) (car clause) (list (car clause)))))
               (unless (every (function integerp) keys) (%fpga-hls-reject "CASE keys must be integers"))
               (push (list keys (%fpga-hls-lower-expression (second clause) environment)) states))))
       (list :fsm selector (nreverse states) (or default (list :constant 0)))))
    ((eq (car form) (quote let))
     (unless (= (length form) 3) (%fpga-hls-reject "LET requires one body expression"))
     (let ((lowered-bindings nil))
       (dolist (binding (second form))
         (unless (and (consp binding) (= (length binding) 2) (symbolp (first binding)))
           (%fpga-hls-reject "invalid LET binding ~S" binding))
         (push (cons (first binding) (%fpga-hls-lower-expression (second binding) environment)) lowered-bindings))
       (%fpga-hls-lower-expression (third form) (append (nreverse lowered-bindings) environment))))
    ((assoc (car form) *fpga-hls-binary-operators*)
     (unless (= (length form) 3) (%fpga-hls-reject "operator ~S requires two operands" (car form)))
     (list :operation (car form)
           (%fpga-hls-lower-expression (second form) environment)
           (%fpga-hls-lower-expression (third form) environment)))
    (t (%fpga-hls-reject "impure or unsupported form ~S" form))))

(defparameter *fpga-hls-operation-delays-ns* (quote ((+ . 1.0) (- . 1.0) (* . 3.0) (logand . 0.5) (logior . 0.5) (logxor . 0.5) (= . 0.5) (< . 0.5) (<= . 0.5) (> . 0.5) (>= . 0.5))))

(defun %fpga-hls-operation-nodes (node)
  (ecase (car node)
    ((:constant :input) nil)
    (:operation (append (%fpga-hls-operation-nodes (third node))
                        (%fpga-hls-operation-nodes (fourth node))
                        (list node)))
    (:mux (append (%fpga-hls-operation-nodes (second node))
                  (%fpga-hls-operation-nodes (third node))
                  (%fpga-hls-operation-nodes (fourth node))))
    (:fsm (append (%fpga-hls-operation-nodes (second node))
                  (mapcan (lambda (state) (%fpga-hls-operation-nodes (second state))) (third node))
                  (%fpga-hls-operation-nodes (fourth node))))))

(defun %fpga-hls-build-schedule (ir resource-limits clock-period-ns)
  (let ((elapsed 0.0) (stage 0) (next-id 0)
        (resource-counts (make-hash-table :test (function eq))) (entries nil))
    (dolist (node (%fpga-hls-operation-nodes ir) (nreverse entries))
      (let* ((operator (second node))
             (delay (or (cdr (assoc operator *fpga-hls-operation-delays-ns*)) 1.0)))
        (when (and clock-period-ns (> (+ elapsed delay) clock-period-ns))
          (incf stage)
          (setf elapsed 0.0))
        (let* ((limit (or (cdr (assoc operator resource-limits)) most-positive-fixnum))
               (ordinal (gethash operator resource-counts 0)))
          (push (list :id next-id :operator operator :stage stage
                      :resource (mod ordinal limit) :start-ns elapsed
                      :finish-ns (+ elapsed delay)) entries)
          (setf (gethash operator resource-counts) (1+ ordinal))
          (incf next-id)
          (incf elapsed delay))))))

(defun %fpga-hls-verilog-expression (node)
  (ecase (car node)
    (:constant (princ-to-string (second node)))
    (:input (third node))
    (:operation
     (format nil "(~A ~A ~A)"
             (%fpga-hls-verilog-expression (third node))
             (case (second node)
               (logand "&") (logior "|") (logxor "^") (= "==") (t (second node)))
             (%fpga-hls-verilog-expression (fourth node))))
    (:mux
     (format nil "(~A ? ~A : ~A)"
             (%fpga-hls-verilog-expression (second node))
             (%fpga-hls-verilog-expression (third node))
             (%fpga-hls-verilog-expression (fourth node))))
    (:fsm
     (reduce (lambda (state fallback)
               (destructuring-bind (keys value) state
                 (reduce (lambda (key nested)
                           (format nil "(~A == ~D ? ~A : ~A)"
                                   (%fpga-hls-verilog-expression (second node)) key
                                   (%fpga-hls-verilog-expression value) nested))
                         keys :from-end t :initial-value fallback)))
             (third node) :from-end t
             :initial-value (%fpga-hls-verilog-expression (fourth node))))))

(defun %fpga-hls-emit-verilog (name parameters ir schedule initiation-interval resource-limits clock-period-ns)
  (declare (ignore initiation-interval resource-limits clock-period-ns))
  (let ((depth (%fpga-hls-pipeline-depth schedule)))
    (with-output-to-string (stream)
      (format stream "module ~A(~{~A, ~}clk, result);~%" name parameters)
      (dolist (parameter parameters)
        (format stream "  input signed [31:0] ~A;~%" parameter))
      (format stream "  input clk;~%  output signed [31:0] result;~%")
      (format stream "  wire signed [31:0] combinational_result;~%")
      (format stream "  assign combinational_result = ~A;~%"
              (%fpga-hls-verilog-expression ir))
      (dotimes (stage depth)
        (format stream "  reg signed [31:0] pipeline_stage_~D;~%" stage))
      (dolist (entry schedule)
        (format stream "  // op ~D: ~A stage ~D resource ~D [~,2F, ~,2F] ns~%"
                (getf entry :id) (getf entry :operator) (getf entry :stage)
                (getf entry :resource) (getf entry :start-ns)
                (getf entry :finish-ns)))
      (format stream "  always @(posedge clk) begin~%")
      (format stream "    pipeline_stage_0 <= combinational_result;~%")
      (loop for stage from 1 below depth
            do (format stream "    pipeline_stage_~D <= pipeline_stage_~D;~%"
                       stage (1- stage)))
      (format stream "  end~%  assign result = pipeline_stage_~D;~%endmodule~%"
              (1- depth)))))

(defun %fpga-hls-pipeline-depth (schedule)
  (if schedule (1+ (reduce (function max) schedule :key (lambda (entry) (getf entry :stage)))) 1))

(defun %fpga-hls-vhdl-expression (node)
  (ecase (car node)
    (:constant (princ-to-string (second node)))
    (:input (third node))
    (:operation
     (format nil "(~A ~A ~A)"
             (%fpga-hls-vhdl-expression (third node))
             (case (second node) (logand "and") (logior "or") (logxor "xor") (= "=") (t (string-downcase (string (second node)))))
             (%fpga-hls-vhdl-expression (fourth node))))
    (:mux
     (format nil "(~A when ~A /= 0 else ~A)"
             (%fpga-hls-vhdl-expression (third node))
             (%fpga-hls-vhdl-expression (second node))
             (%fpga-hls-vhdl-expression (fourth node))))
    (:fsm "0")))

(defun %fpga-hls-emit-vhdl (name parameters ir schedule initiation-interval resource-limits clock-period-ns)
  (declare (ignore initiation-interval resource-limits clock-period-ns))
  (let ((depth (%fpga-hls-pipeline-depth schedule)))
    (with-output-to-string (stream)
      (format stream "library ieee;~%use ieee.std_logic_1164.all;~%~%")
      (format stream "entity ~A is~%  port (~%    clk : in std_logic;~%" name)
      (dolist (parameter parameters)
        (format stream "    ~A : in integer;~%" parameter))
      (format stream "    result : out integer);~%end entity;~%~%")
      (format stream "architecture rtl of ~A is~%" name)
      (dotimes (stage depth)
        (format stream "  signal pipeline_stage_~D : integer;~%" stage))
      (format stream "begin~%  process(clk)~%  begin~%    if rising_edge(clk) then~%")
      (format stream "      pipeline_stage_0 <= ~A;~%" (%fpga-hls-vhdl-expression ir))
      (loop for stage from 1 below depth
            do (format stream "      pipeline_stage_~D <= pipeline_stage_~D;~%"
                       stage (1- stage)))
      (format stream "    end if;~%  end process;~%  result <= pipeline_stage_~D;~%"
              (1- depth))
      (dolist (entry schedule)
        (format stream "  -- op ~D: ~A stage ~D resource ~D [~,2F, ~,2F] ns~%"
                (getf entry :id) (getf entry :operator) (getf entry :stage)
                (getf entry :resource) (getf entry :start-ns)
                (getf entry :finish-ns)))
      (format stream "end architecture;~%"))))

(defun lower-fpga-hls (name lambda-form &key (initiation-interval 1) resource-limits clock-period-ns)
  "Lower a restricted LAMBDA into scheduled FPGA IR and Verilog/VHDL artifacts."
  (%fpga-hls-validate-options initiation-interval resource-limits clock-period-ns)
  (unless (and (consp lambda-form)
               (eq (car lambda-form) (quote lambda))
               (= (length lambda-form) 3)
               (listp (second lambda-form)))
    (%fpga-hls-reject "expected (LAMBDA (parameters...) expression)"))
  (let ((seen nil)
        (seen-names nil)
        (parameters nil))
    (dolist (parameter (second lambda-form))
      (unless (and (symbolp parameter)
                   (not (keywordp parameter))
                   (not (member parameter seen)))
        (%fpga-hls-reject "invalid or duplicate parameter ~S" parameter))
      (let ((parameter-name (%fpga-hls-name parameter)))
        (when (member parameter-name seen-names :test (function string=))
          (%fpga-hls-reject "parameters normalize to duplicate FPGA name ~S" parameter-name))
        (push parameter seen)
        (push parameter-name seen-names)
        (push (list :input parameter parameter-name) parameters)))
    (setf parameters (nreverse parameters))
    (let* ((module-name (%fpga-hls-name name))
           (parameter-names (mapcar (function third) parameters))
           (environment
             (mapcar (lambda (parameter) (cons (second parameter) parameter)) parameters))
           (ir (%fpga-hls-lower-expression (third lambda-form) environment))
           (schedule (%fpga-hls-build-schedule ir resource-limits clock-period-ns))
           (verilog (%fpga-hls-emit-verilog module-name parameter-names ir schedule
                                             initiation-interval resource-limits clock-period-ns))
           (vhdl (%fpga-hls-emit-vhdl module-name parameter-names ir schedule
                                       initiation-interval resource-limits clock-period-ns)))
      (%make-fpga-hls-program
        :name module-name
        :parameters (mapcar (function second) parameters)
        :ir ir
        :schedule schedule
        :verilog verilog
        :vhdl vhdl
        :initiation-interval initiation-interval
        :resource-limits (copy-tree resource-limits)
        :clock-period-ns clock-period-ns))))

(defun emit-fpga-verilog (program &optional stream)
  "Return PROGRAM Verilog, or write it to STREAM and return PROGRAM."
  (check-type program fpga-hls-program)
  (if stream (progn
      (write-string (fpga-hls-program-verilog program) stream)
      program)
    (fpga-hls-program-verilog program)))

(defun emit-fpga-vhdl (program &optional stream)
  "Return PROGRAM VHDL, or write it to STREAM and return PROGRAM."
  (check-type program fpga-hls-program)
  (if stream
      (progn
        (write-string (fpga-hls-program-vhdl program) stream)
        program)
      (fpga-hls-program-vhdl program)))

(export '(fpga-hls-error fpga-hls-error-reason fpga-hls-program fpga-hls-program-name fpga-hls-program-parameters fpga-hls-program-ir fpga-hls-program-schedule fpga-hls-program-verilog fpga-hls-program-vhdl fpga-hls-program-initiation-interval fpga-hls-program-resource-limits fpga-hls-program-clock-period-ns lower-fpga-hls emit-fpga-verilog emit-fpga-vhdl))
