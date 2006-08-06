;; An inline processor node defines a node which expands to a macro
;; which performs some user-defined processing and passes the result
;; to a single handler function.

(in-package :picflow)

(defclass inline-processor (node)
  ((entry-point :reader entry-point
		:initarg :entry-point
		:initform (c-gensym "INLINE"))
   (argument-name :reader argument-name
		  :initarg :argument-name
		  :initform "x"
		  :documentation "The name of the argument to the macro")
   (processor-content :reader processor-content
		      :initarg :processor-content
		      :documentation "The macro-expansion text")))

(defmethod prototype-code-emit ((node inline-processor))
  (emit "#define ~A(~A) ~A"
	(entry-point node)
	(argument-name node)
	(let* ((output-name (car (output-names node)))
	       (output (gethash output-name (outputs node))))
	  (if output
	      (call-output output (processor-content node))
	      (error "Inline processor node ~S needs a single output connection, but has none")))))

(defmacro defcode-inline (name (arg) &body body)
  `(defun ,name () (make-instance 'inline-processor 
		    :argument-name (format nil "~(~A~)" ',arg)
		    :processor-content (progn ,@body)
		    :output-names '(:default)
		    :input-variables '(:default))))

(defcode-inline inverter (x)
  "~(x)")

(defun adder (n)
  "Return a block that adds N to each value"
  (make-instance 'inline-processor 
		 :argument-name "x"
		 :processor-content (format nil "x + ~D" n)
		 :output-names '(:default)
		 :input-variables '(:default)))

(defcode-inline bits-10-to-8 (x)
  "(unsigned char)((x) >> 2)")

(defcode-inline bits-16-to-8 (x)
  "(unsigned char)((x) >> 8)")

(defcode-inline to-unsigned-char (x)
  "(unsigned char)(x)")

(defcode-inline to-unsigned-int (x)
  "(unsigned int)(x)")

(defcode-inline to-unsigned-long (x)
  "(unsigned long)(x)")

(defcode-inline to-char (x)
  "(char)(x)")

(defcode-inline to-int (x)
  "(int)(x)")

(defcode-inline to-long (x)
  "(long)(x)")