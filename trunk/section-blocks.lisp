(in-package :picflow)

;; Prototype section
;;;;;;;;;;;;;;;;;;;;

(defclass prototype-block (node)
  ((code :initarg :code :documentation "The code to be inserted in the prototype section")))

(defmethod prototype-code-emit ((node prototype-block))
  (with-slots (code) node
    (emit code)))

(defun define-prototype-code (code)
  "Insert CODE into the prototype section of the output file"
  (use-node (make-instance 'prototype-block :code code)))

;; Interrupt vector section
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass interrupt-vector-block (node)
  ((code :initarg :code :documentation "The code to be inserted in the interrupt vector section")))

(defmethod interrupt-vector-code-emit ((node interrupt-vector-block))
  (with-slots (code) node
    (emit code)))

(defun define-interrupt-vector-code (code)
  "Insert CODE into the interrupt vector"
  (use-node (make-instance 'interrupt-vector-block :code code)))

;; Init code block
;;;;;;;;;;;;;;;;;;

(defclass init-code-block (node)
  ((code :initarg :code :documentation "The code to be inserted in the init code in the main() function")))

(defmethod init-code-emit ((node init-code-block))
  (with-slots (code) node
    (emit code)))

(defun define-init-code (code)
  "Insert CODE into the main() function for initialization"
  (use-node (make-instance 'init-code-block :code code)))

;; Extra code block
;;;;;;;;;;;;;;;;;;;

(defclass extra-code-block (node)
  ((code :initarg :code :documentation "The code to be inserted in the extra code section")))

(defmethod extra-code-emit ((node extra-code-block))
  (with-slots (code) node
    (emit code)))

(defun define-extra-code (code)
  "Insert CODE into the C file somewhere after the main() function"
  (use-node (make-instance 'extra-code-block :code code)))