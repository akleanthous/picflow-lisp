;; Simple FIR filter support

(in-package :picflow)

(defun simple-fir (&key (in-type "int") (out-type in-type) coeffs)
  "Create a simple FIR filter with given coefficients COEFFS, and
a specified IN-TYPE (a C type specifier string which defaults to
'int') and OUT-TYPE (a C type specifier string which defaults to
IN-TYPE)."
  (assert coeffs) 
  (c-block
   (with-output-to-string (*c-output*)
     (let* ((*indentation-level* 0)
	    (n (length coeffs))
	    (registers (loop repeat n collect (c-gensym "reg"))))
       (emit "~A BLOCKNAME(~A x) {" out-type in-type)
       (with-indent ()
	 (emit "static ~A ~{~a=0~^, ~};~%" in-type registers)
	 (loop for i below n
	    do (apply 'emit "~A = ~A;" 
		      (if (= i (1- n))
			  (list (elt registers i) "x")
			  (list (elt registers i) (elt registers (1+ i))))))
	 (terpri *c-output*)
	 (emit "$DEFAULT(~{(~a)~^ + ~});"
	       (mapcar #'(lambda (b reg) (format nil "~A * ~A" reg b))
		       coeffs registers)))
       (emit "}")))
   :block-name "simple_fir"))