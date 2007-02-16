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

#+nil
(progn
  (cleanup)
  ;;(defparameter tmr0 (make-instance 'timer0 :output-names '(:default) :input-variables '()))
  ;;(defparameter usart (make-instance 'usart-debug :output-names '() :input-variables '(:default)))
  (-> tmr0 ((usart-tracer)))
  (generate-code "18f4520"))


(progn
  (cleanup)
  (defcode-inline fourty-two-adder (x)
    "x + 42")
  (-> ((timer "ticktock" :initial-timer-state 10000))
      ((fourty-two-adder))
      ((usart-tracer)))
  (generate-code "18f4520"))

;; FIXME: this ((foo)) crap is fucking ugly. Reader macros to the
;; rescue! Specifically, some uniform form should be introduced
;; involving, say, [these guys].

(progn
  (cleanup)
  (-> ((timer "flounder_counter")) ((usart-tracer)))
  (-> ((timer "gorbachev" :initial-timer-state 10000)) ((usart-tracer)))
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
    (-> ((timer "periodic_resetter" :initial-timer-state 150)) (pwm2 :in :duty-cycle)))
  (generate-code "18f4520"))

;; C block demo

(defun counter-block ()
  (c-block "
void BLOCKNAME(unsigned long arg) {
  static unsigned int counter = 0;
  $DEFAULT(counter++);
}
"
	   :block-name "counter"))

(progn
  (cleanup)
  (-> ((timer "timekeeper")) ((counter-block)) ((usart-tracer)))
  (generate-code "18f4520"))

(progn
  (cleanup)
  (-> ((timer "timekeeper")) ((counter-block)) ((usart-tracer)))
  (generate-code "18f4680"))

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
  $DEFAULT(counter += increment);
  increment *= 2;
}
"
	   :block-name "wonky_counter"))

;; Derivative block test
(progn
  (cleanup)
  (-> ((timer "timekeeper")) ((wonky-counter-block)) ((derivative "unsigned int")) ((usart-tracer)))
  (generate-code "18f4520"))

;; Integral block test
(progn
  (cleanup)
  (-> ((timer "timekeeper")) ((wonky-counter-block))
      ;; These should cancel each other out, so the next bit can be
      ;; commented out without affecting anything except the first
      ;; term of the sequence. If the integral is put after the
      ;; derivative block, it must have a C term of 1.
      ((integral-rectangle "unsigned int"))
      ;((usart-tracer :printf-string "{int: %u}\\n"))
      ((derivative "unsigned int"))
      ((usart-tracer)))
  (generate-code "18f4520"))

;; Moving average block test
(progn
  (cleanup)
  (-> ((timer "timekeeper" :initial-timer-state 500)) ((counter-block))
      ((moving-average 5 "unsigned char" "unsigned int"))
      ((usart-tracer)))
  (generate-code "18f4520"))

(progn
  (cleanup)
  (-> ((timer "timekeeper" :initial-timer-state 500)) ((wonky-counter-block))
      ((moving-average 5 "unsigned int" "unsigned long"))
      ((usart-tracer)))
  (generate-code "18f4520"))

;; C block input test
(defun increment-counter-block ()
  (c-block "
void BLOCKNAME(unsigned long arg) {
  $DEFAULT($[COUNTER]{unsigned int}{1}++);
}
"
	   :block-name "counter"))

(progn
  (cleanup)
  (let ((counter (increment-counter-block)))
    (-> ((timer "timekeeper")) counter ((usart-tracer)))
    (-> ((timer "periodic_resetter" :initial-timer-state 150)) (counter :in :counter)))
  (generate-code "18f4520"))

;; ADC test
(progn
  (cleanup)
  (-> *an0* ((usart-tracer :type "unsigned int" :printf-string "AN0> %u\\n")))
  (-> *an2* ((usart-tracer :type "unsigned int" :printf-string "AN2> %u\\n")))
  (-> *an1* ((usart-tracer :type "unsigned int" :printf-string "AN1> %u\\n")))
  (-> *an3* ((usart-tracer :type "unsigned int" :printf-string "AN3> %u\\n")))
  (generate-code "18f4520"))

;; Try for PID

(cleanup)

(defcode-inline clear-sum-block (x)
  ())

(defun pid-controller (type-specifier p-gain i-gain d-gain)
  (let ((p-gain-block (fixed-gain p-gain))
	(i-gain-block (fixed-gain i-gain))
	(d-gain-block (fixed-gain d-gain))
	(i-block (integral-rectangle type-specifier))
	(d-block (derivative type-specifier))
	())))

(let ((p-gain (fixed-gain ))))

;; Test the FIR stuff
(progn
  (cleanup)
  (-> ((timer "timekeeper")) ((counter-block)) 
      ((simple-fir :coeffs '(1 2 3)))
      ((usart-tracer)))
  (generate-code "18f4520"))