;; timer0 block

(in-package :picflow)

;; FIXME: this should use a number of counter variables, one for each
;; block, which would be decremented at every tick, with the ticking
;; driven by a timer interrupt. Perhaps use Timer3 for this and make
;; Timer0 more configurable.

(defclass timer0 (node)
  ((isr-symbol :reader isr-symbol :initform (c-gensym "timer0_isr"))
   (entry-point :reader entry-point
		:initarg :entry-point
		:initform "null_entry_point")))

(defmethod interrupt-vector-code-emit ((node timer0))
  (emit "if (INTCONbits.TMR0IF) ~A();" (isr-symbol node)))

(defmethod prototype-code-emit ((node timer0))
  (emit "void ~A(void);  // Timer0 interrupt handler" (isr-symbol node)))

(defmethod init-code-emit ((node timer0))
  (emit "OpenTimer0(TIMER_INT_ON & T0_SOURCE_INT & T0_16BIT & T0_PS_1_32);"))

(defmethod extra-code-emit ((node timer0))
  (emit "// Timer0 interrupt handler")
  (emit "void ~A(void) {" (isr-symbol node))
  (with-indent ()
    (emit "INTCONbits.TMR0IF = 0;")
    (dolist (output-name (output-names node))
      (let ((output-node (gethash output-name (outputs node))))
	(when output-node
	  (emit "~A(0);" (entry-point output-node))))))
  (emit "}~%"))

(defparameter tmr0 (make-instance 'timer0 :output-names '(:default) :input-variables '()))