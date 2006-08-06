(in-package :picflow)

(defclass output-port (node)
  ((letter :initarg :letter :reader port-letter
	   :documentation "Port letter. Must be uppercase. Symbols like 'b work nicely.")
   (entry-point :reader entry-point
		:initarg :entry-point
		:initform (c-gensym "output_port_handler"))
   (tris-bits :initarg :tris-bits :reader tris-bits :initform 0)))

(defmethod prototype-code-emit ((node output-port))
  (emit "void ~A(unsigned char arg);  // Port handler" (entry-point node)))

(defmethod init-code-emit ((node output-port))
  (emit "TRIS~A = 0x~x;" (port-letter node) (tris-bits node))
  (emit "LAT~A = 0x00;" (port-letter node)))

(defmethod extra-code-emit ((node output-port))
  (emit "// Port output handler")
  (emit "void ~A(unsigned char arg) {" (entry-point node))
  (with-indent ()
    (emit "// Any input pins will be unaffected by this operation")
    (emit "LAT~A = arg;" (port-letter node)))
  (emit "}~%"))

(defparameter lata (make-instance 'output-port :letter 'a :output-names '() :input-variables '(:default)))
(defparameter latb (make-instance 'output-port :letter 'b :output-names '() :input-variables '(:default)))
(defparameter latc (make-instance 'output-port :letter 'c :output-names '() :input-variables '(:default)))
(defparameter latd (make-instance 'output-port :letter 'd :output-names '() :input-variables '(:default)))