(in-package :picflow)

(defstruct analog-output-handler
  "A record for an output from the ADC daemon"
  (output       :type output-record)
  (input-number :type (integer 0 7)))

(defclass adc-daemon (node)
  ((adc-callback :initform (c-gensym "adc_callback") :reader adc-callback)
   (output-handlers :initform '() :accessor output-handlers))
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
  (emit "setADC~D();" (analog-output-handler-input-number (car (output-handlers node))))
  (emit-file "include/adc_init.inc"))

(defmethod extra-code-emit ((node adc-daemon))
  (emit "// Callback activated by ADC completion")
  (emit "void ~A(void) {" (adc-callback node))
  (with-indent ()
    (emit-file "include/adc_callback_top.inc")
    (loop for remaining-handlers on (output-handlers node)
	  for index from 0
	  for handler = (car remaining-handlers)
	  for at-end-p = (null (cdr remaining-handlers))
	  do (progn
	       (emit "case ~D:  // AN~D" index (analog-output-handler-input-number handler))
	       (with-indent ()
		 (emit (call-output (analog-output-handler-output handler) "adres_tenbits"))
		 (if at-end-p
		     (emit "adc_output_state = 0;")
		     (emit "adc_output_state++;"))
		 (let ((next-input-number (analog-output-handler-input-number
					   (if at-end-p
					       (car (output-handlers node))
					       (cadr remaining-handlers)))))
		   (emit "setADC~D();" next-input-number))
		 (emit "break;"))))
    (emit "} // end of switch-case block")
    (emit-file "include/adc_callback_boilerplate.inc"))
  (emit "}"))

(defparameter *adc-daemon* (make-instance 'adc-daemon)
  "Global ADC daemon. There can be only one.")

(defun cleanup-adc-daemon ()
  "Reset the ADC daemon"
  (setf *adc-daemon* (make-instance 'adc-daemon)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew 'cleanup-adc-daemon *cleanup-functions*))


;; Actual user-usable code for analog pins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass analog-pin (node)
  ((input-number :initarg :input-number :reader analog-pin-input-number :initform 0
		 :documentation "The number of the analog input pin. 3
for AN3, 6 for AN6, and so on."))
  (:documentation "An analog input pin. Outputs 10-bit analog values repeatedly."))

(defmethod notify-> ((node analog-pin) (b node))
  ;; Analog pins need the ADC daemon to support them
  (use-node *adc-daemon*)
  ;; Add the default (and hopefully only) output linkage to the list of
  ;; output handlers for the ADC daemon.
  (pushnew (make-analog-output-handler :input-number (analog-pin-input-number node)
				       :output (gethash :default (outputs node)))
	   (output-handlers *adc-daemon*)
	   :key #'analog-output-handler-input-number))

(defun new-analog-pin (input-number)
  "Return a new analog pin associated with input number INPUT-NUMBER"
  (make-instance 'analog-pin 
		 :output-names '(:default) 
		 :input-names  '(:default) 
		 :input-number input-number))

(defparameter *an0* (new-analog-pin 0))
(defparameter *an1* (new-analog-pin 1))
(defparameter *an2* (new-analog-pin 2))
(defparameter *an3* (new-analog-pin 3))
(defparameter *an4* (new-analog-pin 4))
(defparameter *an5* (new-analog-pin 5))
(defparameter *an6* (new-analog-pin 6))
(defparameter *an7* (new-analog-pin 7))