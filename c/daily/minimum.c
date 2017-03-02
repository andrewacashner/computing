/* Recursively find minimum of array, 2015/10/14 */

int minimum(int *array, int size)
{
  int i, min, next, *array_ptr;
  min = array[0];
  array_ptr = array;
  for (i = 0; i < size; ++i) {
    next = minimum(array_ptr + 1, size - 1);
    if (min >= next) {
      return(next);
    }
  }
  return(min);
}
