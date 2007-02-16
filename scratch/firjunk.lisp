
;; FIR filters
;;;;;;;;;;;;;;

;; First, I need some simple Lisp code for testing

(defun fir-lookback-filter (coeffs signal)
  "Process signal using a FIR filter that looks backwards in time
  and inserts leading zeros into its output. Does not work properly."
  (let ((n (length coeffs)))
    (collecting
      ;; Insert leading zeros
      (loop repeat n
	 do (collect 0))
      (loop for idx from (1- n) below (length signal)
	 do (collect (loop for x below n
			sum (* (elt (reverse coeffs) x)
			       (elt signal (- idx x)))))))))

(fir-lookback-filter '(1) '(1 2 3 4 5 6 7 8 9))
(fir-lookback-filter '(1 1) '(1 2 3 4 5 6 7 8 9))
(fir-lookback-filter '(1 2) '(1 2 3 4 5 6 7 8 9))
(fir-lookback-filter '(0.5 0.5) '(1 5 4 6 10 2))

(fir-lookback-filter '(0.33333 0.33333 0.33333) '(1 5 4 6 10 2 1 1 4 12 1 1 1 4 4 4 1 76 3))

;; FIR filter, coeffs [1/3, 1/3, 1/3]
(let ((a 0) (b 0) (c 0))
  (defun filtor (x)
    (setf a b)
    (setf b c)
    (setf c x)
    (+ (* 1/3 a) (* 1/3 b) (* 1/3 c))))

(defmacro make-filtor (name (&rest coeffs))
  (let* ((n (length coeffs))
	 (registers (loop repeat n collect (gensym "reg"))))
    (with-unique-names (x)
      `(let ,(mapcar #'(lambda (reg) (list reg 0)) registers)
	 (defun ,name (,x)
	   ,@(loop for i below n
		  collect (if (= i (1- n))
			      `(setf ,(elt registers i) ,x)
			      `(setf ,(elt registers i) ,(elt registers (1+ i)))))
	   (+ ,@(mapcar #'(lambda (b reg) (list '* b reg))
			coeffs registers)))))))



(make-filtor avg2 (0.5 0.5))

(make-filtor sfir (1 2 3))

(loop for x in '(1 5 4 6 10 2 1 1 4 12 1 1 1 4 4 4 1 76 3)
     collect (filtor x))
