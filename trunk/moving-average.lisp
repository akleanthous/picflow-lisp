;; N-element moving average block

(in-package :picflow)

(defun moving-average (n &optional (type-specifier "unsigned char") (intermediate-type "unsigned int"))
  "Return a block which takes inputs and produces outputs of a type
given by the string TYPE-SPECIFIER, which outputs a moving average of
the last N values passed to the block. The string INTERMEDIATE-TYPE
speifies a type big enough to hold the sum of N values passed to the
block, and it is used for an intermediate value in the averaging
process. Each block activation occurs in constant time, irrespective
of the value of N, and the block takes storage proportional to N."
  (c-block (format nil "
void BLOCKNAME(~A arg) {
  // Storage for the values which will be averaged
  static ~:*~A values[~A];
  // The index in the array in which this incoming value will be placed
  static unsigned char value_index = 0;
  // The sum of all elements currently in the array
  static ~A sum = 0;
  // A boolean variable: is the array full?
  static unsigned char array_full = 0;

  sum += arg;
  if (array_full) {
    sum -= values[value_index];
    values[value_index] = arg;
    $DEFAULT(sum / ~:*~:*~A);
  } else {
    values[value_index] = arg;
  }

  // Go on to the next array position
  value_index++;

  // Wrap around to beginning of array if we're at the end
  if (value_index >= ~:*~A) {
    value_index = 0;
    array_full = 1;
    $DEFAULT(sum / ~:*~A);
  }
}" type-specifier n intermediate-type)
	   :block-name "moving_average"))