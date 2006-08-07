;; FIXME: get some better tests set up

(in-package :picflow)

;; Loading code
(eval-when (:execute :compile-toplevel :load-toplevel)
  (cl:load "picflow.asd")
  (asdf:oos 'asdf:load-op :picflow))

#+nil
(progn
  (cleanup)
  ;;(defparameter tmr0 (make-instance 'timer0 :output-names '(:default) :input-variables '()))
  ;;(defparameter lata (make-instance 'output-port :letter 'a :output-names '() :input-variables '(:default)))
  (-> tmr0 lata)
  (generate-code "18f4520"))

(progn
  (cleanup)
  ;;(defparameter tmr0 (make-instance 'timer0 :output-names '(:default) :input-variables '()))
  ;;(defparameter usart (make-instance 'usart-debug :output-names '() :input-variables '(:default)))
  (-> tmr0 usart-debug-node)
  (generate-code "18f4520"))

(progn
  (cleanup)
  (defcode-inline fourty-two-adder (x)
    "x + 42")
  (-> tmr0 (fourty-two-adder) usart-debug-node)
  (generate-code "18f4520"))

;; FIXME: this ((foo)) crap is fucking ugly. Reader macros to the
;; rescue! Specifically, some uniform form should be introduced
;; involving, say, [these guys].

(progn
  (cleanup)
  (-> ((timer "flounder_counter")) usart-debug-node)
  (-> ((timer "gorbachev" :initial-timer-state 10000)) usart-debug-node)
  (generate-code "18f4520"))

(progn
  ;; The current frequency is about 77 Hz
  (cleanup)
  (pwm "LATBbits.LATB3" "dc1" :initial-duty-cycle 128)
  (pwm "LATBbits.LATB3" "dc2" :initial-duty-cycle 64)
  (generate-code "18f4520"))

;; Set up two PWM outputs, one of which has its duty cycle
;; periodically set to the initial timer state of a timer. This is a
;; Rube Goldberg contraption set up to test the linkage code and
;; individual components.
(progn
  ;; The current frequency is about 77 Hz
  (cleanup)
  (pwm "LATBbits.LATB3" "dc1" :initial-duty-cycle 128)
  (let ((pwm2 (pwm "LATBbits.LATB3" "dc2" :initial-duty-cycle 64)))
    (-> ((timer "periodic_resetter" :initial-timer-state 150)) (pwm2 :in '|dc2|)))
  (generate-code "18f4520"))

;; C block demo

(defun counter-block ()
  (c-block "
void BLOCKNAME(unsigned long arg) {
  static unsigned int counter = 0;
  $DEFAULT(counter++);
}
"))

(progn
  (cleanup)
  (-> ((timer "timekeeper")) ((counter-block)) usart-debug-node)
  (generate-code "18f4520"))

(progn
  (cleanup)
  (-> ((timer "timekeeper")) ((counter-block)) ((usart-tracer)))
  (generate-code "18f4520"))

(progn
  (cleanup)
  (-> ((timer "timekeeper")) ((counter-block))
      ((to-unsigned-int))
      ((usart-tracer :type "unsigned int" :printf-string "> %u  "))
      ((usart-tracer :type "unsigned int" :printf-string "[%u]\\n")))
  (generate-code "18f4520"))

;; Splitter test
(progn
  (cleanup)
  (let ((splitty (splitter "unsigned int" 'a 'b)))
    (-> ((timer "timekeeper")) ((counter-block))
	((to-unsigned-int))
	splitty)
    (-> (splitty :out 'a) ((usart-tracer :type "unsigned int" :printf-string "> %u\\n")))
    (-> (splitty :out 'b) ((usart-tracer :type "unsigned int" :printf-string "[%u]\\n"))))
  (generate-code "18f4520"))

;; Section block test
(progn
  (cleanup)
  (define-prototype-code "// PROTOTYPE CODE")
  (define-interrupt-vector-code "// IV CODE")
  (define-init-code "// INIT CODE")
  (define-extra-code "// THIS IS EXTRA CODE, YO!")
  (set-main-loop-code "// Do the main loop! Loop mainly!")
  (generate-code "18f4520"))

(defun wonky-counter-block ()
  (c-block "
void BLOCKNAME(unsigned long arg) {
  static unsigned int counter = 0;
  static unsigned int increment = 1;
  $DEFAULT(counter + increment);
  increment *= 2;
}
"))

;; Derivative block test
(progn
  (cleanup)
  (-> ((timer "timekeeper")) ((wonky-counter-block)) ((derivative "unsigned int")) ((usart-tracer)))
  (generate-code "18f4520"))