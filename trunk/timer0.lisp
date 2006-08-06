(in-package :picflow)

(defclass timer0 (node)
  ((isr-symbol :reader isr-symbol :initform (c-gensym "timer0_isr"))
   (entry-point :reader entry-point :initarg :entry-point :initform "null_entry_point")
   (output-nodes :accessor output-nodes :initarg :output-nodes :initform '()
		 :documentation "A list of software timer nodes to
which timer0 is linked. Yes, I know this doesn't obey standard linkage
rules. And I don't care. The coupling between timer0 and
software-timer is strong, and I see no point in loosening it.")))

(defmethod interrupt-vector-code-emit ((node timer0))
  (emit "if (INTCONbits.TMR0IF) ~A();" (isr-symbol node)))

(defmethod prototype-code-emit ((node timer0))
  (emit "void ~A(void);  // Timer0 interrupt handler" (isr-symbol node)))

(defmethod init-code-emit ((node timer0))
  (emit "OpenTimer0(TIMER_INT_ON & T0_SOURCE_INT & T0_8BIT & T0_PS_1_1);"))

(defmethod extra-code-emit ((node timer0))
  (emit "// Timer0 interrupt handler")
  (emit "void ~A(void) {" (isr-symbol node))
  (with-indent ()
    (emit "INTCONbits.TMR0IF = 0;")
    (dolist (output-node (output-nodes node))
      (with-slots (isr-symbol counter-variable initial-timer-state auto-restore-init-state-p) output-node
	(if auto-restore-init-state-p
	    ;; Assignment before call so called code can change counter value too
	    (emit "if (--~A <= 0) { ~A = ~A; ~A(); }"
		  counter-variable counter-variable initial-timer-state isr-symbol)
	    (emit "if (--~A <= 0) ~A();" counter-variable isr-symbol)))))
  (emit "}~%"))

(defparameter *global-timer0* (make-instance 'timer0))

(defun cleanup-timer0 ()
  "Delete any Timer0 state. This is a cleanup function."
  (setf *global-timer0* (make-instance 'timer0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'cleanup-timer0 *cleanup-functions* :test #'eql))


;; Software timers built on top of Timer0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass software-timer (node)
  ((isr-symbol :reader isr-symbol :initform (c-gensym "sw_timer_isr"))
   (entry-point :reader entry-point :initarg :entry-point :initform "null_entry_point")
   (counter-variable :reader counter-variable :initarg :counter-variable :initform (c-gensym "counter"))
   (initial-timer-state :reader initial-timer-state :initarg :initial-timer-state :initform 50)
   (auto-restore-init-state-p :reader auto-restore-init-state-p :initarg :auto-restore-init-state-p :initform t))
  (:documentation "A timer defined in software, powered by Timer0 and a counter variable"))

(defmethod initialize-instance :after ((node software-timer) &key)
  (pushnew node (output-nodes *global-timer0*) :test #'eq))

(defmethod prototype-code-emit ((node software-timer))
  (emit "void ~A(void);  // A software timer pseudo-interrupt handler" (isr-symbol node))
  (emit "unsigned long ~A = ~A; // Counter for a software timer" 
	(counter-variable node)
	(initial-timer-state node)))

(defmethod extra-code-emit ((node software-timer))
  (emit "// A software timer interrupt handler")
  (emit "void ~A(void) {" (isr-symbol node))
  (with-indent ()
    (dolist (output-name (output-names node))
      (let ((output (gethash output-name (outputs node))))
	(when output
	  (emit (call-output output (counter-variable node)))))))
  (emit "}~%"))

(defmethod use-node ((node software-timer))
  (use-node *global-timer0*)
  (call-next-method))

(defun timer (counter-variable &key (initial-timer-state 5000) (auto-restore-init-state-p t))
  "Return a new software timer object with a given initial timer state
which will count down once each tick of the Timer0 clock. The counter
for the timer will be put in a variable named by COUNTER-VARIABLE,
which will be declared as an unsigned long. If
AUTO-RESTORE-INIT-STATE-P is true, the initial state will be restored
every time the output is called."
  (make-instance 'software-timer
		 :counter-variable counter-variable
		 :initial-timer-state initial-timer-state
		 :auto-restore-init-state-p auto-restore-init-state-p
		 :output-names '(:default) :input-variables '()))