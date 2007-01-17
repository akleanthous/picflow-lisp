(in-package :picflow)

;; (config-bits
;;  (a/d :an1)
;;  (trisb 0)
;;  (trisd 0))

;; (defcode-inline inverter (x)
;;   "~x")

;; (-> an1 latb)
;; (-> an1 inverter latd)			; Equivalent to (-> an1 inverter) (-> inverter latd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nodes* '()
  "A list of all the nodes in the current application")

(defclass node ()
  ((outputs :initform (make-hash-table)
	    :accessor outputs
	    :documentation "A hash table mapping output slot names to output records")
   (output-names :type list
		 :documentation "A list of output slot names available"
		 :accessor output-names
		 :initarg :output-names
		 :initform '())
   (inputs :initform (make-hash-table)
	   :accessor inputs
	   :documentation "A hash table mapping input slot names to C variable names")
   (input-names :type list
		:documentation "A list of variable names for
global input variables. These should be symbols, and :default has a
special meaning here: it means that the node has a standard input, and
does not specify an input variable. The symbols are transformed into C
variable names by looking them up in the INPUTS hash table."
		    :accessor input-names
		    :initarg :input-names
		    :initform '())
   (entry-point :reader entry-point
		:initarg :entry-point
		:initform "null_entry_point")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *cleanup-functions* '()))

(defun cleanup ()
  (setf *nodes* nil)
  (mapc #'funcall *cleanup-functions*))

(defmethod use-node ((node node))
  (pushnew node *nodes* :test #'eq))

(defmethod valid-input-p ((node node) input-name)
  "Is the symbol INPUT-NAME a valid input name for NODE?"
  (check-type input-name symbol)
  (or (null input-name)
      (member (string input-name) (input-names node) :test #'equalp :key #'string)))

(defmethod valid-output-p ((node node) output-name)
  "Is the symbol INPUT-NAME a valid output name for NODE?"
  (check-type output-name symbol)
  (or (null output-name)
      (member (string output-name) (output-names node) :test #'equalp :key #'string)))

;; Output records
;;;;;;;;;;;;;;;;;

(defstruct output-record
  "A struct describing an outgoing link from one node to another. It
carries information about which node is being linked to what, and how."
  (source-node nil :type node)
  (node nil :type node)
  (output-type nil :type symbol)
  ;; Extra information will be used in linkage-type-dependant ways
  (extra-information nil))

(defun call-output (output arg &key (semicolon-p t))
  "Return C code to call an output record. A trailing semicolon is
added by default, but this can be overridden by specifying SEMICOLON-P
to be nil."
  (check-type output output-record)
  (concatenate 'string
	       (ecase (output-record-output-type output)
		 (:normal (format nil "~A(~A)"
				  (entry-point (output-record-node output))
				  arg))
		 (:variable (format nil "~A = ~A"
				    (lookup-input-variable-name (output-record-node output)
								(output-record-extra-information output))
				    arg)))
	       (if semicolon-p ";" "")))

;; Input records
;;;;;;;;;;;;;;;;

(defmethod lookup-input-variable-name ((node node) name-symbol)
  "Given a node and a symbol which is a valid input name for that
node, return the C variable name string corresponding to the symbol."
  ;; FIXME: should there be a default dead-end variable? Or just return nil?
  (gethash name-symbol (inputs node)))

;; Linkage code
;;;;;;;;;;;;;;;

;; (-> (foo :in inpname :out outname))

(defun %normalize-linkage-form (form &key (default-input-name :default) (default-output-name :default))
  "Given a list or symbol FORM, returns a linkage descriptor in
normalized (object :in input-name :out output-name) form. The default
input-name is :default, as is the default output-name, but both of
these can be overridden (and, if they are first or last in the linkage
chain, SHOULD be overridden) by the DEFAULT-INPUT-NAME and
DEFAULT-OUTPUT-NAME keyword arguments. This also performs checking to
ensure that input-name and output-name, if given, are symbols and
valid input or outputs for the object."
  (let ((form (if (listp form) form (list form)))) ; Turn raw objects into (object) lists.
    (let ((object (or (car form)
		      (error "object linkage descriptor ~A should not be empty" form)))
	  (input-name (getf (cdr form) :in default-input-name))
	  (output-name (getf (cdr form) :out default-output-name)))
      ;; Check that the objects all have the right types
      (check-type object node "a node")
      (check-type input-name symbol)
      (check-type output-name symbol)
      ;; Check that input-name and output-name are valid for the object
      (unless (valid-input-p object input-name)
	(error "~S is not a valid input name for the node ~S" input-name object))
      (unless (valid-output-p object output-name)
	(error "~S is not a valid output name for the node ~S" output-name object))
      ;; Finally return the thing in normal form!
      (list object :in input-name :out output-name))))

(defmacro -> (&rest nodes)
  (labels ((normalize-node (node)
	     (if (listp node)
		 (destructuring-bind (object &key (in nil in?) (out nil out?)) node
		   (let ((in-form  (if in?  `(:in ,in)))
			 (out-form (if out? `(:out ,out))))
		     `(list ,object ,@in-form ,@out-form)))
		 node)))
    `(fun-> ,@(mapcar #'normalize-node nodes))))

(defun fun-> (&rest nodes)
  ;; Make sure we have two or more nodes to link
  (assert (> (length nodes) 1) (nodes)
	  "-> function needs two or more nodes to link, and less were given in the form ~S" (cons '-> nodes))
  ;; Normalize NODES to (object :in input-name :out output-name)
  ;; form. The first and last nodes of the chain have nil default
  ;; input and output names.
  (let ((nodes `(,(%normalize-linkage-form (first nodes) :default-input-name nil)
		 ,@(mapcar '%normalize-linkage-form (subseq nodes 1 (1- (length nodes))))
		 ,(%normalize-linkage-form (car (last nodes)) :default-output-name nil))))
    ;; Add all nodes in the chain to the list of used nodes
    (mapc (compose #'use-node #'car) nodes)
    ;; Connect the nodes together with METHOD->
    (labels ((call-method-> (a-form b-form)
	       (destructuring-bind (a-object &key in out) a-form
		 (let ((a-in in) (a-out out)) ; Hack to get things named properly
		   (destructuring-bind (b-object &key in out) b-form
		     (let ((b-in in) (b-out out)) ; Hack to get things named properly
		       (method-> a-object a-in a-out a-form b-object b-in b-out b-form)))))))
      (reduce #'call-method-> nodes))))

(defgeneric method-> (a a-in a-out a-form b b-in b-out b-form)
  (:documentation "Link A to B using designated input and output
slots. The proper linkage forms are also provided for the convenience
of any methods which may try to override the default behavior: calling
prim-> on the linkage forms."))

(defmethod method-> ((a node) a-in a-out a-form (b node) b-in b-out b-form)
  ;; By default, call prim-> on the linkage forms
  (declare (ignore a-in a-out b-in b-out))
  (prim-> a-form b-form))

(defun prim-> (a-form b-form)
  "Connect two nodes, a to b, given normalized linkage forms. Return
the form for b, to make this easier to apply with REDUCE."
  (destructuring-bind (a-object &key in out) a-form
    (let ((a-in in) (a-out out))	; Hack to get things named properly
      (destructuring-bind (b-object &key in out) b-form
	(let ((b-in in) (b-out out))	; Hack to get things named properly
	  (declare (ignore a-in b-out))
	  (let ((record (if (equalp (symbol-name b-in) "DEFAULT")
			    (make-output-record :source-node a-object :node b-object :output-type :normal)
			    (make-output-record :source-node a-object :node b-object :output-type :variable
						;; Extra information is the variable name symbol in this case
						:extra-information b-in))))
	    (setf (gethash a-out (outputs a-object)) record))))))
  b-form)

;; C generation code
;;;;;;;;;;;;;;;;;;;;

(defparameter *c-output* *standard-output*)
(defvar *indentation-level* 0
  "The number of levels to indent the code")

(defmacro with-indent ((&key (spaces 4)) &body body)
  `(let ((*indentation-level* (+ *indentation-level* ,spaces)))
    ,@body))

(defun emit (fmt &rest args)
  "Emit to C output with proper newlines and indentation. Same arguments as FORMAT."
  (labels ((spaces (n)
	     (with-output-to-string (str)
	       (loop repeat n do (write-char #\Space str)))))
    (apply #'format *c-output* (concatenate 'string "~&" (spaces *indentation-level*) fmt "~%") args)))

(defun emit-file (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil nil)
	  while line
	  do (emit line))))

(defun std-c-headers (pic-type)
  "Emit a standard set of C header files for the PIC named in
pic-type, e.g., '18f4520' or '18f4431'"
  ;; First emit the PIC-specific header file, e.g., "#include <p18f4520.h>"
  (emit "#include <p~A.h>" pic-type)
  ;; Then emit the device-independant code, found in an include file
  (emit-file "include/std-c-headers.inc"))

(defvar *c-gensym-counter* 0
  "A counter used to generate probably-unique C symbols")

(defun cleanup-c-gensym-counter ()
  "Reset the C gensym counter to 0 on seperate runs for aesthetics"
  (setf *c-gensym-counter* 0))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew 'cleanup-c-gensym-counter *cleanup-functions*))

(defun c-gensym (name)
  "Generate a probably-unique C symbol"
  (prog1 (format nil "~(~A~)_G~S" name *c-gensym-counter*)
    (incf *c-gensym-counter*)))

;; Interrupt vector code emission
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric interrupt-vector-code-emit (node)
  (:documentation "Emit interrupt vector code for a node. This will be
placed into the interrupt vector at 0x18 in no guaranteed order."))

(defmethod interrupt-vector-code-emit ((node node))
  "A node, by default, emits no interrupt vector code."
  nil)

(defun interrupt-vector-emit ()
  "Emit the code for the interrupt vector at 0x18, inserting code from
all objects in *nodes*."
  (emit-file "include/interrupt-vector.inc")
  (with-indent ()
    (mapc #'interrupt-vector-code-emit *nodes*))
  (emit "}~%"))

;; Prototype declaration emission
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric prototype-code-emit (node)
  (:documentation "Emit global variable and function prototype code
for a node. This will be placed near the top of the generated C
file"))

(defmethod prototype-code-emit ((node node))
  "A node, by default, emits no global variable or function prototype
code."  
  #+nil (emit "// Node: ~S" node)
  nil)

(defun prototype-emit ()
  "Emit the code for the global variables and function prototypes for
all nodes"
  (emit-file "include/prototype-comment.inc")
  (emit "void interruptHandlerLow(void);")
  (mapc #'prototype-code-emit *nodes*))

;; Initialization code emission
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric init-code-emit (node)
  (:documentation "Emit init code for a node. This will be placed in
the main() function"))

(defmethod init-code-emit ((node node))
  "A node, by default, emits no init code."
  nil)

(defun init-emit ()
  "Emit the init code for all nodes. This applies no indentation, so
if you want a nice-looking main() function you had best indent it
externally."
  (emit-file "include/init-comment.inc")
  (mapc #'init-code-emit *nodes*))

;; Extra code emission
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric extra-code-emit (node)
  (:documentation "Emit extra code for a node. This will be placed in
the main() function"))

(defmethod extra-code-emit ((node node))
  "A node, by default, emits no extra code."
  nil)

(defun extra-emit ()
  "Emit the extra code for all nodes. This applies no indentation, so
if you want a nice-looking main() function you had best indent it
externally."
  (emit-file "include/extra-comment.inc")
  (mapc #'extra-code-emit *nodes*))

;; Main loop code setting/clearing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *main-loop-code* "// No-op"
  "The code to be inserted into the while (1) {...} loop in the main() function")

(defun cleanup-main-loop-code ()
  (setf *main-loop-code* "// No-op"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'cleanup-main-loop-code *cleanup-functions*))

(defun set-main-loop-code (code)
  "Set CODE to be run in the while (1) {...} loop in the main() function"
  (setf *main-loop-code* code))

;; Main code generation
;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-code (pic)
  "Generate the main body of code"
  (emit-file "include/heading-comment.inc")
  (std-c-headers pic)
  (emit "")
  (prototype-emit)
  (emit "")
  (interrupt-vector-emit)
  (emit "~%void main(void) {")
  (with-indent ()
    (init-emit)
    (emit "")
    (emit "// Enable interrupts and loop")
    (emit "INTCONbits.GIE = 1;")
    (emit "INTCONbits.PEIE = 1;")
    (emit "while (1) {")
    (with-indent () 
      (emit *main-loop-code*))
    (emit "}"))
  (emit "}~%")
  (extra-emit))