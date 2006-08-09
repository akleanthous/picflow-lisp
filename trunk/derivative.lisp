;; The derivative block differentiates with respect to time.

(in-package :picflow)

(defun derivative (type-specifier)
  "Returns a derivative with respect to time block, which takes and
passes on a numeric argument of a type specified by the type-specifier
string."
  (c-block (format nil "void BLOCKNAME(~A arg) {
  static ~:*~A counter = 0;
  static ~:*~A last_value = 0;
  static unsigned char have_last_value = 0;
  overlay ~:*~A derivative;

  if (have_last_value) {
    derivative = arg - last_value;
    last_value = arg;
    $DEFAULT(derivative);
  } else {
    last_value = arg;
    have_last_value = 1;
  }
}" type-specifier)
	   :block-name "derivative"))