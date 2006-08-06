(in-package :picflow)

;; Note: the proper TRIS bits need to be set seperately. This is a FIXME.
;; In fact, this whole module needs reworking.

(defclass pwm-output-aux (node)
  ((latch-bit :initarg :latch-bit :reader latch-bit
	      :documentation "A string such as 'LATBbits.LATB3' determining the output latch bit")
   (entry-point :reader entry-point :initarg :entry-point :initform (c-gensym "pwm_output_handler"))
   ;; FIXME: use an input instead! Or ditch the input system entirely.
   (duty-cycle-variable :initarg :duty-cycle-variable :reader duty-cycle-variable)
   (initial-duty-cycle :initarg :initial-duty-cycle :reader initial-duty-cycle :initform 0)
   (counter-variable :reader counter-variable :initarg :counter-variable
		     :documentation "The same variable used by the software timer")))

(defmethod prototype-code-emit ((node pwm-output-aux))
  (emit "unsigned char ~A = ~A;" (duty-cycle-variable node) (initial-duty-cycle node)))

(defmethod init-code-emit ((node pwm-output-aux))
  (emit "~A = 0;" (latch-bit node)))

(defmethod extra-code-emit ((node pwm-output-aux))
  (emit "// PWM output auxilliary node event handler. arg will be discarded.")
  (emit "void ~A(unsigned long arg) {" (entry-point node))
  (with-indent ()
    (emit "if (~A) {" (latch-bit node))
    (with-indent ()
      (emit "if (~A < 0xFF) ~A = 0;" (duty-cycle-variable node) (latch-bit node))
      (emit "~A = 0xFF - ~A;" (counter-variable node) (duty-cycle-variable node)))
    (emit "} else {")
    (with-indent ()
      (emit "if (~A > 0) ~A = 1;" (duty-cycle-variable node) (latch-bit node))
      (emit "~A = ~A;" (counter-variable node) (duty-cycle-variable node)))
    (emit "}"))
  (emit "}~%"))

(defun pwm (latch-bit duty-cycle-variable-name &key (initial-duty-cycle 128))
  "Return a PWM output block which has already had its input connected
to a software timer block which will be created silently. It will
twiddle the bit specified by the C expression in LATCH-BIT, which
should be a variable such as 'LATBbits.LATB3'. Its duty cycle will be
controlled by a variable named in the string argument
DUTY-CYCLE-VARIABLE-NAME, and the :INITIAL-DUTY-CYCLE argument
specifies the initial 8-bit duty cycle. The default is 128, or
50%. Returns pwm-block, timer-block."
  (let* ((counter-variable (c-gensym "pwm_timer_counter"))
	 (timer-block (timer counter-variable :initial-timer-state 10 :auto-restore-init-state-p nil))
	 (pwm-block (make-instance 'pwm-output-aux
				   :latch-bit latch-bit
				   :duty-cycle-variable (make-symbol duty-cycle-variable-name)
				   :initial-duty-cycle initial-duty-cycle
				   :counter-variable counter-variable
				   :output-names '()
				   :input-variables `(:default ,(make-symbol duty-cycle-variable-name)))))
    (-> timer-block pwm-block)
    (values pwm-block timer-block)))
