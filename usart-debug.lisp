;; USART debug output

(in-package :picflow)

(defclass usart-debug (node)
  ((entry-point :reader entry-point
		:initarg :entry-point
		:initform (c-gensym "usart_debug_handler"))
   (arg-type :reader usart-debug-arg-type
	     :initarg :arg-type
	     :initform '("int" . "%i\\n")
	     :documentation "A cons of (type . printf-string) where
type is a string like 'unsigned int' specifying the type of the
argument to the entry point and printf-string is a string such as '%i' specifying
the format string passed to printf, which is given the argument to
print. A newline is not included and must be inserted in the printf-string.")))

(defmethod use-node :after ((node usart-debug))
  (use-node *usart-config*))

(defmethod prototype-code-emit ((node usart-debug))
  (emit "void ~A(~A arg);  // Usart-debug handler"
	(entry-point node) (car (usart-debug-arg-type node))))

(defmethod extra-code-emit ((node usart-debug))
  (emit "// Usart-debug handler")
  (emit "void ~A(~A arg) {" (entry-point node) (car (usart-debug-arg-type node)))
  (with-indent ()
    (emit "printf(\"~A\", arg);" (cdr (usart-debug-arg-type node)))
    (multiple-value-bind (output foundp)
	(gethash :default (outputs node))
      (when foundp
	(emit (call-output output "arg")))))
  (emit "}~%"))

;; FIXME: this shouldn't even be here, but some progs use it. It's deprecated.
(defparameter usart-debug-node (make-instance 'usart-debug :output-names '(:default) :input-variables '(:default)))

(defun usart-tracer (&key (type "int") (printf-string "%i\\n"))
  "Return a usart-debug node which prints out and passes along its
input where type is a string like 'unsigned int' specifying the type
of the argument to the entry point and printf-string is a string such
as '%i' specifying the format string passed to printf, which is given
the argument to print. A newline is not included and must be inserted
in the printf-string. The default is int."
  (make-instance 'usart-debug 
		 :output-names '(:default) 
		 :input-variables '(:default)
		 :arg-type (cons type printf-string)))
