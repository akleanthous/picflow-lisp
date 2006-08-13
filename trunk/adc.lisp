(in-package :picflow)

(defclass adc-daemon (node)
  ((adc-callback :initform (c-gensym "adc_callback") :reader adc-callback))
  (:documentation "This node handles global setup and scaffolding for
analog-to-digital conversion. It is not used directly, but instead
supports other nodes."))

(defmethod interrupt-vector-code-emit ((node adc-daemon))
  (emit "if (PIR1bits.ADIF) ~A();" (adc-callback node)))

(defmethod prototype-code-emit ((node adc-daemon))
  (emit "#include \"picflow_adc.h\"")
  (emit "void ~A(void);" (adc-callback node)))

(defmethod init-code-emit ((node adc-daemon))
  ;; FIXME: set up first channel
  (emit-file "include/adc_init.inc"))

(defmethod extra-code-emit ((node adc-daemon))
  (emit "// Callback activated by ADC completion")
  (emit "void ~A(void) {" (adc-callback node))
  (with-indent ()
    (emit "overlay unsigned int adres_tenbits = TENBITS(ADRESH, ADRESL);~%")
    ;; FIXME: this linkage mechanism is wrong
    (let ((output (gethash :default (outputs node))))
      (emit (if output
		(call-output output "adres_tenbits")
		"#warning \"Nothing done with ADC values\"")))
    (emit-file "include/adc_callback_boilerplate.inc"))
  (emit "}"))

;; Actual user-usable code for analog pins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass analog-pin (node)
  ((input-number :initarg :input-number :reader analog-pin-input-number :initform 0
		 :documentation "The number of the analog input pin. 3
for AN3, 6 for AN6, and so on.")
   (entry-point :reader entry-point :initarg :entry-point :initform (c-gensym "analog_pin")))
  (:documentation "An analog input pin. Outputs 10-bit analog values repeatedly."))

(defmethod prototype-code-emit ((node analog-pin))
  (emit "#define ~A(_arg) ~A"
	(let* ((output-name (car (output-names node)))
	       (output (gethash output-name (outputs node))))
	  (if output
	      (call-output output "")
	      (error "Analog pin node ~S needs a single output connection, but has none")))))
