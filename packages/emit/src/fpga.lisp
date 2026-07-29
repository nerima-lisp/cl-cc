(in-package :cl-cc/emit)

(define-condition fpga-hls-error (error) ((reason :initarg :reason :reader fpga-hls-error-reason)) (:report (lambda (condition stream) (format stream "FPGA HLS rejected input: ~A" (fpga-hls-error-reason condition)))))

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

(progn
  (defun %fpga-hls-reference-stage (reference schedule)
    (if (eq (car reference) :value)
        (getf (find (second reference) schedule :key (lambda (entry) (getf entry :id))) :stage)
        -1))
  (defun %fpga-hls-build-schedule (ir resource-limits clock-period-ns initiation-interval)
    (let ((entries nil) (next-id 0)
          (next-unlimited-resource (make-hash-table :test (function eq)))
          (resource-stage-residues (make-hash-table :test (function equal))))
      (labels ((entry-stage (reference) (%fpga-hls-reference-stage reference entries))
               (compatible-stage (operator resource earliest)
                 (let ((used (gethash (list operator resource) resource-stage-residues)))
                   (loop for stage from earliest
                         unless (member (mod stage initiation-interval) used)
                           return stage
                         when (>= (- stage earliest) (1- initiation-interval))
                           return nil)))
               (new-entry (kind stage &rest properties)
                 (let ((entry (list* :id next-id :kind kind :stage stage properties)))
                   (incf next-id) (push entry entries) (list :value (getf entry :id))))
               (walk (node)
                 (ecase (car node)
                   ((:constant :input) node)
                   (:operation
                    (let* ((operator (second node)) (lhs (walk (third node))) (rhs (walk (fourth node)))
                           (delay (or (cdr (assoc operator *fpga-hls-operation-delays-ns*)) 1.0))
                           (earliest (1+ (max (entry-stage lhs) (entry-stage rhs))))
                           (limit (cdr (assoc operator resource-limits)))
                           (resource nil)
                           (stage nil))
                      (if limit
                          (loop with best-stage = most-positive-fixnum
                                for candidate below limit
                                for candidate-stage = (compatible-stage operator candidate earliest)
                                when (and candidate-stage (< candidate-stage best-stage))
                                  do (setf resource candidate stage candidate-stage
                                           best-stage candidate-stage)
                                finally
                                   (unless resource
                                     (%fpga-hls-reject
                                       "resource limit for ~S cannot satisfy initiation interval ~D"
                                       operator initiation-interval)))
                          (progn
                            (setf resource (gethash operator next-unlimited-resource 0))
                            (setf stage earliest)
                            (incf (gethash operator next-unlimited-resource 0))))
                      (when (and clock-period-ns (> delay clock-period-ns))
                        (%fpga-hls-reject "operator ~S delay ~,2F ns exceeds clock period ~,2F ns"
                                          operator delay clock-period-ns))
                      (push (mod stage initiation-interval)
                            (gethash (list operator resource) resource-stage-residues))
                      (new-entry :operation stage :operator operator :resource resource :lhs lhs :rhs rhs
                                 :start-ns 0.0 :finish-ns delay)))
                   (:mux
                    (let* ((selector (walk (second node))) (then-value (walk (third node)))
                           (else-value (walk (fourth node)))
                           (stage (1+ (max (entry-stage selector) (entry-stage then-value)
                                          (entry-stage else-value)))))
                      (new-entry :mux stage :selector selector :then then-value :else else-value)))
                   (:fsm
                    (let* ((selector (walk (second node)))
                           (states (mapcar (lambda (state) (list (first state) (walk (second state))))
                                           (third node)))
                           (default (walk (fourth node)))
                           (stage (1+ (reduce (function max) states
                                             :key (lambda (state) (entry-stage (second state)))
                                             :initial-value (max (entry-stage selector)
                                                                 (entry-stage default))))))
                      (new-entry :fsm stage :selector selector :states states :default default))))))
        (walk ir)
        (nreverse entries)))))

(progn
  (defun %fpga-hls-pipeline-depth (schedule)
    (if schedule (1+ (reduce (function max) schedule :key (lambda (entry) (getf entry :stage)))) 1))

  (defun %fpga-hls-result-reference (ir schedule)
    (if schedule (list :value (getf (car (last schedule)) :id)) ir))

  (defun %fpga-hls-verilog-reference (reference &optional stage)
    (ecase (car reference)
      (:constant (princ-to-string (second reference)))
      (:input (if stage (format nil "~A_stage_~D" (third reference) stage) (third reference)))
      (:value (format nil "op_~D_reg" (second reference)))))

  (defun %fpga-hls-vhdl-reference (reference &optional stage)
    (%fpga-hls-verilog-reference reference stage))

  (defun %fpga-hls-verilog-operator (operator)
    (case operator (logand "&") (logior "|") (logxor "^") (= "==") (t (string-downcase (string operator)))))

  (defun %fpga-hls-vhdl-operator-expression (operator lhs rhs)
    (case operator
      (logand (format nil "to_integer(to_signed(~A, 32) and to_signed(~A, 32))" lhs rhs))
      (logior (format nil "to_integer(to_signed(~A, 32) or to_signed(~A, 32))" lhs rhs))
      (logxor (format nil "to_integer(to_signed(~A, 32) xor to_signed(~A, 32))" lhs rhs))
      ((= < <= > >=) (format nil "boolean'pos(~A ~A ~A)" lhs
                              (string-downcase (string operator)) rhs))
      (t (format nil "(~A ~A ~A)" lhs (string-downcase (string operator)) rhs))))

  (defun %fpga-hls-unit-name (operator resource)
    (format nil "~A_fu_~D" (case operator (+ "add") (- "sub") (* "mul") (logand "and")
                                    (logior "or") (logxor "xor") (= "eq") (< "lt")
                                    (<= "le") (> "gt") (>= "ge")) resource))

  (defun %fpga-hls-operation-units (schedule)
    (remove-duplicates
      (loop for entry in schedule when (eq (getf entry :kind) :operation)
            collect (list (getf entry :operator) (getf entry :resource)))
      :test (function equal)))

  (defun %fpga-hls-unit-entries (unit schedule)
    (remove-if-not
      (lambda (entry)
        (and (eq (getf entry :kind) :operation)
             (equal unit (list (getf entry :operator) (getf entry :resource)))))
      schedule))

  (defun %fpga-hls-verilog-stage-selection (entries key)
    (reduce (lambda (entry fallback)
              (format nil "pipeline_valid[~D] ? ~A : ~A"
                      (getf entry :stage)
                      (%fpga-hls-verilog-reference (getf entry key) (getf entry :stage))
                      fallback))
            entries :from-end t :initial-value "0"))

  (defun %fpga-hls-vhdl-stage-selection (entries key)
    (format nil "~{~A~^ + ~}"
            (mapcar
              (lambda (entry)
                (format nil "boolean'pos(pipeline_valid(~D) = '1') * (~A)"
                        (getf entry :stage)
                        (%fpga-hls-vhdl-reference (getf entry key) (getf entry :stage))))
              entries)))

  (defun %fpga-hls-verilog-entry-expression (entry)
    (let ((stage (getf entry :stage)))
      (ecase (getf entry :kind)
        (:mux (format nil "(~A != 0) ? ~A : ~A"
                      (%fpga-hls-verilog-reference (getf entry :selector) stage)
                      (%fpga-hls-verilog-reference (getf entry :then) stage)
                      (%fpga-hls-verilog-reference (getf entry :else) stage)))
        (:fsm
         (reduce (lambda (state fallback)
                   (reduce (lambda (key nested)
                             (format nil "(~A == ~D) ? ~A : ~A"
                                     (%fpga-hls-verilog-reference (getf entry :selector) stage) key
                                     (%fpga-hls-verilog-reference (second state) stage) nested))
                           (first state) :from-end t :initial-value fallback))
                 (getf entry :states) :from-end t
                 :initial-value (%fpga-hls-verilog-reference (getf entry :default) stage))))))

  (defun %fpga-hls-vhdl-entry-expression (entry)
    (let ((stage (getf entry :stage)))
      (ecase (getf entry :kind)
        (:mux (format nil "boolean'pos(~A /= 0) * (~A) + boolean'pos(~A = 0) * (~A)"
                      (%fpga-hls-vhdl-reference (getf entry :selector) stage)
                      (%fpga-hls-vhdl-reference (getf entry :then) stage)
                      (%fpga-hls-vhdl-reference (getf entry :selector) stage)
                      (%fpga-hls-vhdl-reference (getf entry :else) stage)))
        (:fsm
         (let* ((selector (%fpga-hls-vhdl-reference (getf entry :selector) stage))
                (keys (mapcan (lambda (state) (copy-list (first state))) (getf entry :states)))
                (terms
                  (loop for state in (getf entry :states) append
                    (loop for key in (first state)
                          collect (format nil "boolean'pos(~A = ~D) * (~A)" selector key
                                          (%fpga-hls-vhdl-reference (second state) stage))))))
           (format nil "~{~A~^ + ~} + boolean'pos(~{~A /= ~D~^ and ~}) * (~A)"
                   terms
                   (loop for key in keys append (list selector key))
                   (%fpga-hls-vhdl-reference (getf entry :default) stage)))))))

  (defun %fpga-hls-emit-verilog (name parameters ir schedule initiation-interval resource-limits clock-period-ns)
    (declare (ignore resource-limits clock-period-ns))
    (let ((depth (%fpga-hls-pipeline-depth schedule)))
      (with-output-to-string (stream)
        (format stream "module ~A(~{~A, ~}clk, result, result_valid);~%" name parameters)
        (dolist (parameter parameters) (format stream "  input signed [31:0] ~A;~%" parameter))
        (format stream "  input clk;~%  output signed [31:0] result;~%  output result_valid;~%")
        (when schedule
          (format stream "  integer ii_counter = 0;~%")
          (format stream "  reg [~D:0] pipeline_valid = 0;~%  reg result_valid_reg = 0;~%" (1- depth))
          (dolist (parameter parameters)
            (dotimes (stage depth) (format stream "  reg signed [31:0] ~A_stage_~D;~%" parameter stage)))
          (dolist (entry schedule) (format stream "  reg signed [31:0] op_~D_reg;~%" (getf entry :id)))
          (dolist (unit (%fpga-hls-operation-units schedule))
            (destructuring-bind (operator resource) unit
              (let ((unit-name (%fpga-hls-unit-name operator resource))
                    (entries (%fpga-hls-unit-entries unit schedule)))
                (format stream "  wire signed [31:0] ~A_lhs, ~A_rhs, ~A_result;~%" unit-name unit-name unit-name)
                (format stream "  assign ~A_lhs = ~A;~%" unit-name (%fpga-hls-verilog-stage-selection entries :lhs))
                (format stream "  assign ~A_rhs = ~A;~%" unit-name (%fpga-hls-verilog-stage-selection entries :rhs))
                (format stream "  assign ~A_result = (~A_lhs ~A ~A_rhs);~%" unit-name unit-name
                        (%fpga-hls-verilog-operator operator) unit-name))))
          (format stream "  always @(posedge clk) begin~%")
          (format stream "    result_valid_reg <= pipeline_valid[~D];~%" (1- depth))
          (format stream "    pipeline_valid <= pipeline_valid << 1;~%    pipeline_valid[0] <= 0;~%")
          (dolist (parameter parameters)
            (loop for stage from 1 below depth do
              (format stream "    ~A_stage_~D <= ~A_stage_~D;~%" parameter stage parameter (1- stage))))
          (format stream "    if (ii_counter == 0) begin~%")
          (dolist (parameter parameters) (format stream "      ~A_stage_0 <= ~A;~%" parameter parameter))
          (format stream "      pipeline_valid[0] <= 1;~%      ii_counter <= ~D;~%    end else begin~%      ii_counter <= ii_counter - 1;~%    end~%" (1- initiation-interval))
          (dolist (entry schedule)
            (format stream "    if (pipeline_valid[~D]) op_~D_reg <= ~A;~%"
                    (getf entry :stage) (getf entry :id)
                    (if (eq (getf entry :kind) :operation)
                        (format nil "~A_result" (%fpga-hls-unit-name (getf entry :operator) (getf entry :resource)))
                        (%fpga-hls-verilog-entry-expression entry))))
          (format stream "  end~%"))
        (format stream "  assign result_valid = ~A;~%" (if schedule "result_valid_reg" "1'b1"))
        (format stream "  assign result = ~A;~%endmodule~%"
                (%fpga-hls-verilog-reference (%fpga-hls-result-reference ir schedule))))))

  (defun %fpga-hls-emit-vhdl (name parameters ir schedule initiation-interval resource-limits clock-period-ns)
    (declare (ignore resource-limits clock-period-ns))
    (let ((depth (%fpga-hls-pipeline-depth schedule)))
      (with-output-to-string (stream)
        (format stream "library ieee;~%use ieee.std_logic_1164.all;~%use ieee.numeric_std.all;~%~%")
        (format stream "entity ~A is~%  port (~%    clk : in std_logic;~%" name)
        (dolist (parameter parameters) (format stream "    ~A : in integer;~%" parameter))
        (format stream "    result : out integer;~%    result_valid : out std_logic);~%end entity;~%~%architecture rtl of ~A is~%" name)
        (when schedule
          (format stream "  signal ii_counter : integer range 0 to ~D := 0;~%" (1- initiation-interval))
          (format stream "  signal pipeline_valid : std_logic_vector(~D downto 0) := (others => '0');~%  signal result_valid_reg : std_logic := '0';~%" (1- depth))
          (dolist (parameter parameters)
            (dotimes (stage depth) (format stream "  signal ~A_stage_~D : integer := 0;~%" parameter stage)))
          (dolist (entry schedule) (format stream "  signal op_~D_reg : integer := 0;~%" (getf entry :id)))
          (dolist (unit (%fpga-hls-operation-units schedule))
            (let ((unit-name (%fpga-hls-unit-name (first unit) (second unit))))
              (format stream "  signal ~A_lhs, ~A_rhs, ~A_result : integer;~%" unit-name unit-name unit-name)))
          (format stream "begin~%")
          (dolist (unit (%fpga-hls-operation-units schedule))
            (destructuring-bind (operator resource) unit
              (let ((unit-name (%fpga-hls-unit-name operator resource))
                    (entries (%fpga-hls-unit-entries unit schedule)))
                (format stream "  ~A_lhs <= ~A;~%" unit-name (%fpga-hls-vhdl-stage-selection entries :lhs))
                (format stream "  ~A_rhs <= ~A;~%" unit-name (%fpga-hls-vhdl-stage-selection entries :rhs))
                (format stream "  ~A_result <= ~A;~%" unit-name
                        (%fpga-hls-vhdl-operator-expression operator (format nil "~A_lhs" unit-name)
                                                            (format nil "~A_rhs" unit-name))))))
          (format stream "  process(clk)~%  begin~%    if rising_edge(clk) then~%")
          (format stream "      result_valid_reg <= pipeline_valid(~D);~%      pipeline_valid(0) <= '0';~%" (1- depth))
          (loop for stage from 1 below depth do
            (format stream "      pipeline_valid(~D) <= pipeline_valid(~D);~%" stage (1- stage)))
          (dolist (parameter parameters)
            (loop for stage from 1 below depth do
              (format stream "      ~A_stage_~D <= ~A_stage_~D;~%" parameter stage parameter (1- stage))))
          (format stream "      if ii_counter = 0 then~%")
          (dolist (parameter parameters) (format stream "        ~A_stage_0 <= ~A;~%" parameter parameter))
          (format stream "        pipeline_valid(0) <= '1'; ii_counter <= ~D;~%      else~%        ii_counter <= ii_counter - 1;~%      end if;~%" (1- initiation-interval))
          (dolist (entry schedule)
            (format stream "        if pipeline_valid(~D) = '1' then op_~D_reg <= ~A; end if;~%"
                    (getf entry :stage) (getf entry :id)
                    (if (eq (getf entry :kind) :operation)
                        (format nil "~A_result" (%fpga-hls-unit-name (getf entry :operator) (getf entry :resource)))
                        (%fpga-hls-vhdl-entry-expression entry))))
          (format stream "    end if;~%  end process;~%"))
        (unless schedule (format stream "begin~%"))
        (when schedule (format stream "  result_valid <= result_valid_reg;~%"))
        (unless schedule (format stream "  result_valid <= '1';~%"))
        (format stream "  result <= ~A;~%end architecture;~%"
                (%fpga-hls-vhdl-reference (%fpga-hls-result-reference ir schedule)))))))

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
           (schedule (%fpga-hls-build-schedule ir resource-limits clock-period-ns
                                                initiation-interval))
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
