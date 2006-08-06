(in-package :picflow)

;; Syntax of the proto-C that is given to the C blocks
;; ===================================================
;; 
;; The proto-C must define a function named BLOCKNAME. It should take
;; one numeric input, and it should not return anything (that is, its
;; return type should be void). 
;; 
;; The function has outputs and can read global input variables which
;; are implicitly defined when you use them. To call an output in
;; proto-C, use the syntax "$outputname(argument)", where outputname
;; is the (case-sensitive) name of the output and argument is a valid
;; C expression for the argument to the output. The default output
;; name is default, and this is not case-sensitive (so you can spell
;; it DEFAULT or DeFaULt).
;; 
;; To read an input, use the syntax "$[inputname]", where inputname is
;; the (also case-sensitive) name of the input. The variable type
;; defaults to int, but you can specify the type by referring to the
;; input variable with the syntax "$[inputname]{type}" the first time
;; it is used. If multiple conflicting type declarations are given,
;; all but the first are quietly ignored. If the type is not declared
;; the first time an input variable is used, any subsequent type
;; declarations will be ignored!
;; 
;; // Example proto-C code
;; void BLOCKNAME(int input) {
;;   printf("Custom C block called! Flower number: %i\n", $[flowernumber]{unsigned int});
;;   $answer(42);
;;   $default($[flowernumber]);
;; }

(defclass c-block-node (node)
  ((entry-point :reader entry-point
		:initarg :entry-point
		:initform (c-gensym "cBlock"))
   (text :reader c-block-text
	 :initarg :text
	 :documentation "The unprocessed text of the C expansion.")
   (inputs :accessor c-block-inputs
	   :initarg :inputs
	   :documentation "The input names and types as a list of
(name . type) conses, where the names are non-upcased keyword symbols
and the types are C type specifier strings.")))

(defun c-block (text)
  (let ((node (make-instance 'c-block-node :text text))
	(inputs (extract-inputs text)))
    (setf (output-names node)    (extract-outputs text))
    (setf (input-variables node) (cons :default (mapcar #'car inputs)))
    (setf (c-block-inputs node)  inputs)
    node))

(defun extract-outputs (text)
  "Given a block of unprocessed proto-C, extract the output names and
return them as a list of interned symbols in the KEYWORD package. Upcasing is not
performed."
  (remove-duplicates (mapcar #'(lambda (name)
				 (intern (subseq name 1) "KEYWORD"))
			     (cl-ppcre:all-matches-as-strings "\\$[a-zA-Z_]+(?=\\s*\\()" text))
		     :test #'equalp :from-end t))

(defun matched-parens-filter (pos)
  "Match a region of text enclosed by balanced parentheses"
  (when (< pos cl-ppcre::*end-pos*)
    ;; Only start matching at an opening paren
    (when (char= (aref cl-ppcre::*string* pos) #\()
      (let ((current-pos (1+ pos))
	    (paren-level 1))
	(loop while (and (> paren-level 0)
			 (< current-pos cl-ppcre::*end-pos*))
	      do (progn
		   (case (aref cl-ppcre::*string* current-pos)
		     (#\( (incf paren-level))
		     (#\) (decf paren-level)))
		   (incf current-pos)))
	(if (and (<= current-pos cl-ppcre::*end-pos*)
		 (= paren-level 0))
	    current-pos
	    nil)))))

(defun replace-outputs (text node)
  "Given a block of unprocessed proto-C, replace the output names with code to call the outputs"
  (cl-ppcre:regex-replace-all '(:SEQUENCE #\$
				(:REGISTER
				 (:GREEDY-REPETITION 1 NIL
				  (:CHAR-CLASS (:RANGE #\a #\z) (:RANGE #\A #\Z) #\_)))
				(:GREEDY-REPETITION 0 NIL :WHITESPACE-CHAR-CLASS)
				(:REGISTER (:FILTER matched-parens-filter)))
			      text
			      #'(lambda (match output-name arg)
				  (declare (ignore match))
				  (setf output-name (intern output-name "KEYWORD"))
				  (multiple-value-bind (output-record foundp)
				      (gethash output-name (outputs node))
				    (if foundp
					(call-output output-record arg :semicolon-p nil)
					(progn
					  (warn "Output ~S not connected from ~S" output-name node)
					  (format nil "null_entry_point~A" arg)))))
			      :simple-calls t))

(defun replace-blockname (text name)
  "Replaces all instances of 'BLOCKNAME' in TEXT with NAME, returning TEXT"
  (cl-ppcre:regex-replace-all "BLOCKNAME" text name))

(defun extract-inputs (text)
  "Given a block of unprocessed proto-C, extract the input names and
types and return them as a list of (name . type) conses, where the
names are non-upcased symbols in the KEYWORD package and the types are
C type specifier strings."
  (let ((uses (collecting
		(cl-ppcre:do-matches-as-strings (match "\\$\\[([a-zA-Z_]+)\\](\\{([^}]*)\\})?" text)
		  (cl-ppcre:register-groups-bind (name worthless-gibberish type)
		      ("\\$\\[([a-zA-Z_]+)\\](\\{([^}]*)\\})?" match)
		    (declare (ignore worthless-gibberish))
		    (collect (cons (intern name "KEYWORD") (or type "int"))))))))
    (remove-duplicates uses :test #'equalp :key #'car :from-end t)))

(defun replace-inputs (text)
  "Given a block of unprocessed proto-C, replace the input variables
with code actually referencing the variables"
  (cl-ppcre:regex-replace-all "\\$\\[([a-zA-Z_]+)\\](\\{([^}]*)\\})?" text "\\1"))

(defparameter *sample-code* "void BLOCKNAME(int input) {
  printf(\"Custom C block called! Flower number: %i\n\", $[flowernumber]{unsigned int});
  $answer(42); $steveThePirate($[fool_input]);
  $default($[flowernumber]);
}")

;; Standard node API methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod prototype-code-emit ((node c-block-node))
  (emit "// Declarations for ~S" node)
  (loop for (input-name . input-type) in (c-block-inputs node)
	do (emit "~A ~A;" input-type input-name)))

(defmethod extra-code-emit ((node c-block-node))
  (emit (replace-outputs (replace-inputs (replace-blockname (c-block-text node)
							    (entry-point node)))
			 node)))

#+nil
(c-block *sample-code*)