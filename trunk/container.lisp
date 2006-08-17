(in-package :picflow)

(defclass container (node)
  ((entry-point :reader entry-point
		:initarg :entry-point
		:initform (c-gensym "container"))
   (input-redirects :accessor input-redirects
		    :initarg :input-redirects
		    :initform '()
		    :documentation "A list of (this-name other-name
object) records, where this-name is the name of the input slot for
this block, other-name is the name of the input slot on the other
block to which the input redirects, and object is the other object.")
   (output-redirects :accessor output-redirects
		     :initarg :output-redirects
		     :initform '()
		     :documentation "A list of (this-name other-name
object) records, where this-name is the name of the output slot for
this block, other-name is the name of the output slot on the other
block to which the input redirects, and object is the other object."))
  (:documentation "A block that contains other blocks and redirects
its inputs and outputs to those of certain blocks within it."))
