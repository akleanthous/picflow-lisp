void BLOCKNAME(num_t arg) {
  // Storage for the values which will be averaged
  static num_t values[N_ELEMENTS];
  // The index in the array in which this incoming value will be placed
  static unsigned char value_index = 0;
  // The sum of all elements currently in the array
  static numsum_t sum = 0;
  // A boolean variable: is the array full?
  static unsigned char array_full = 0;

  sum += arg;
  if (array_full) {
    sum -= values[value_index];
    values[value_index] = arg;
    $DEFAULT(sum / N_ELEMENTS);
  } else {
    values[value_index] = arg;
  }

  // Go on to the next array position
  value_index++;

  // Wrap around to beginning of array if we're at the end
  if (value_index >= N_ELEMENTS) {
    value_index = 0;
    array_full = 1;
  }
}
