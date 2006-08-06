(in-package :picflow)

(defclass splitter-block (node)
  ((entry-point :reader entry-point
		:initarg :entry-point
		:initform (c-gensym "splitter"))
   (arg-type :reader splitter-block-arg-type
	     :initarg :arg-type
	     :initform "int"
	     :documentation "A C type specifier string determining
what type of value this block takes. Can also be a symbol."))
  (:documentation "A block that has one input and multiple outputs"))

(defmethod prototype-code-emit ((node splitter-block))
  (emit "void ~A(~(~A~) arg);" (entry-point node) (splitter-block-arg-type node)))

(defmethod extra-code-emit ((node splitter-block))
  (emit "// Splitter node")
  (emit "void ~A(~(~A~) arg) {" (entry-point node) (splitter-block-arg-type node))
  (with-indent ()
    (dolist (output-name (output-names node))
      (let ((output (gethash output-name (outputs node))))
	(when output
	  (emit (call-output output "arg"))))))
  (emit "}"))

(defun splitter (type-specifier &rest output-names)
  "Return a splitter with given output names which takes an argument
of the C type given in type-specifier"
  (make-instance 'splitter-block 
		 :arg-type type-specifier
		 :output-names output-names
		 :input-variables '(:default)))