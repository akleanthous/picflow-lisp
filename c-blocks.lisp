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
;; To read an input, use the syntax "$[INPUTNAME]", where INPUTNAME is
;; the (also case-sensitive) name of the input. It should be
;; uppercase, but this is not mandatory. The variable type defaults to
;; int, but you can specify the type and default value by referring to
;; the input variable with the syntax
;; "$[INPUTNAME]{type}{default-value}" the first time it is used. If
;; multiple conflicting type declarations are given, all but the first
;; are quietly ignored. If the type is not declared the first time an
;; input variable is used, any subsequent type declarations will be
;; ignored!
;; 
;; // Example proto-C code
;; void BLOCKNAME(int input) {
;;   printf("Custom C block called! Flower number: %i\n", $[FLOWERNUMBER]{unsigned int}{0});
;;   $answer(42);
;;   $default($[FLOWERNUMBER]);
;; }

(defclass c-block-node (node)
  ((entry-point :reader entry-point
		:initarg :entry-point
		:initform (c-gensym "cBlock"))
   (text :reader c-block-text
	 :initarg :text
	 :documentation "The unprocessed text of the C expansion.")
   (c-block-inputs :accessor c-block-inputs
		   :initarg :c-block-inputs
		   :documentation "The input names and types as a list
of (name type  default-value) lists, where the names are non-upcased keyword
symbols and the types and default values are C type specifier strings.")))

(defun c-block (text &key (block-name "cBlock"))
  "Return a C block given the proto-C TEXT. You may specify a block
name, from which the real block name will be generated. The block name
defaults to 'cBlock'."
  (let ((node (make-instance 'c-block-node :text text :entry-point (c-gensym block-name)))
	(inputs (extract-inputs text)))
    ;; Extract the output names and set them up
    (setf (output-names node) (extract-outputs text))
    ;; Extract the input names and set them up
    (setf (input-names node) (cons :default (mapcar #'first inputs)))
    ;; Generate a bunch of gensyms for the inputs and set up the
    ;; name-to-gensymmed-name mapping.
    (dolist (input-name (mapcar #'first inputs))
      (setf (gethash input-name (inputs node)) 
	    (c-gensym (symbol-name input-name))))
    ;; Save the inputs data structure for later
    (setf (c-block-inputs node)  inputs)
    ;; Return the node itself for use by user code
    node))

(defun extract-outputs (text)
  "Given a block of unprocessed proto-C, extract the output names and
return them as a list of interned symbols in the KEYWORD package. Upcasing is not
performed."
  (remove-duplicates (mapcar #'(lambda (name)
				 (intern (subseq name 1) "KEYWORD"))
			     (cl-ppcre:all-matches-as-strings "\\$[a-zA-Z_]+(?=\\s*\\()" text))
		     :test #'equalp :from-end t))

;; This function is needed for some of the proto-C parsing. Most of
;; the "parsing" is done with regular expressions, but regexps are
;; notoriously poor at matching balanced parenthetical
;; expressions. This plugin to cl-ppcre adds matching ability for this
;; specific thing.
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
types and return them as a list of (name type default-value) lists,
where the names are non-upcased symbols in the KEYWORD package and the
types are C type specifier strings. You're encouraged to use uppercase
input names, but it's not strictly necessary."
  (let ((uses (collecting
		(cl-ppcre:do-matches-as-strings (match "\\$\\[([a-zA-Z_]+)\\](\\{([^}]*)\\})?(\\{([^}]*)\\})?" text)
		  (cl-ppcre:register-groups-bind (name worthless-gibberish type more-worthless-gibberish default-value)
		      ("\\$\\[([a-zA-Z_]+)\\](\\{([^}]*)\\})?(\\{([^}]*)\\})?" match)
		    (declare (ignore worthless-gibberish more-worthless-gibberish))
		    (collect (list (intern (or name (c-gensym "UNNAMED")) "KEYWORD")
				   (or type "int")
				   (or default-value "0"))))))))
    (remove-duplicates uses :test #'equalp :key #'car :from-end t)))

(defun replace-inputs (text node)
  "Given a block of unprocessed proto-C, replace the input variables
with code actually referencing the variables"
  (cl-ppcre:regex-replace-all "\\$\\[([a-zA-Z_]+)\\](\\{([^}]*)\\})?(\\{([^}]*)\\})?" text 
			      #'(lambda (match input-name &rest worthless-gibberish)
				  (declare (ignore match worthless-gibberish))
				  (lookup-input-variable-name node (intern input-name "KEYWORD")))
			      :simple-calls t))

;; This is some sample code with no purpose other than parser testing.
(defparameter *sample-code* "void BLOCKNAME(int input) {
  printf(\"Custom C block called! Flower number: %i\n\", $[FLOWERNUMBER]{unsigned int}{1});
  $answer(42); $steveThePirate($[FOOL_INPUT]);
  $default($[FLOWERNUMBER]);
}")

;; Standard node API methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod prototype-code-emit ((node c-block-node))
  ;; Note: this doesn't make prototypes for functions defined in the
  ;; proto-C. This should perhaps be changed, but MCC18 doesn't
  ;; complain. Yet.
  (loop for (input-name input-type default-value) in (c-block-inputs node)
	do (emit "~A ~A = ~A;" 
		 input-type 
		 (lookup-input-variable-name node input-name)
		 default-value)))

(defmethod extra-code-emit ((node c-block-node))
  ;; The whole pipeline: process the proto-C when necessary.
  (emit (replace-outputs (replace-inputs (replace-blockname (c-block-text node)
							    (entry-point node))
					 node)
			 node)))

#+nil
(c-block *sample-code*)