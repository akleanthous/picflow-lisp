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

;; FIXME: revert this and do a simpler redirect method

;; (defmethod method-> ((a node) a-in a-out a-form (b container) b-in b-out b-form)
;;   (declare (ignore a-in a-out b-form))
;;   (destructuring-bind (this-name other-name object)
;;       (find b-in (input-redirects b) :key #'first)
;;     (declare (ignore this-name))
;;     (prim-> a-form (list object :in other-name :out b-out))))

;; (defmethod method-> ((a container) a-in a-out a-form (b node) b-in b-out b-form)
;;   (declare (ignore b-out b-in a-form))
;;   (destructuring-bind (this-name other-name object)
;;       (find a-out (output-redirects a) :key #'first)
;;     (declare (ignore this-name))
;;     (prim-> (list object :in a-in :out other-name) b-form)))

;; (defmethod method-> ((a container) a-in a-out a-form (b container) b-in b-out b-form)
;;   (error "FIXME"))