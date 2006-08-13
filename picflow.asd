;; -*-Lisp-*-
(defpackage :picflow-system
  (:use :cl :asdf))
(in-package :picflow-system)

(defsystem picflow
  :name "picflow"
  :author "Peter Scott <pjscott@iastate.edu>"
  :version "0.1"
  :components
  ((:file "package")
   (:file "picflow" :depends-on ("package"))
   (:file "timer0" :depends-on ("picflow"))
   (:file "usart-config" :depends-on ("picflow"))
   (:file "usart-debug" :depends-on ("picflow" "usart-config"))
   (:file "output-ports" :depends-on ("picflow"))
   (:file "inline-processor" :depends-on ("picflow"))
   (:file "pwm" :depends-on ("timer0"))
   (:file "c-blocks" :depends-on ("picflow"))
   (:file "section-blocks" :depends-on ("picflow"))
   (:file "splitter" :depends-on ("picflow"))
   (:file "adc" :depends-on ("picflow"))
   (:file "derivative" :depends-on ("c-blocks"))
   (:file "integral" :depends-on ("c-blocks"))
   (:file "moving-average" :depends-on ("c-blocks")))
  :depends-on (cl-utilities cl-ppcre))