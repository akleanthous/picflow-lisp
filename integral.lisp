;; Various types of integral blocks

(in-package :picflow)

(defun integral-rectangle (type-specifier &optional c)
  "Returns an integral with respect to time block, which takes and
passes on a numeric argument of a type specified by the type-specifier
string. Uses rectangular integration method, which can sometimes be
the best, such as when it is applied to the output of the derivative
block. Adds c (defaults to zero) to its output"
  (c-block (format nil "void BLOCKNAME(~A arg) {
  static ~:*~A value = 0;
  value += arg;
  $DEFAULT(value~:[~; + (~:*~A)~]);
}" type-specifier c)
	   :block-name "integral"))