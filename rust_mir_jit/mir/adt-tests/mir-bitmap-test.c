#include "mir-bitmap.h"

int main (void) {
  int status;
  bitmap_t b1, b2, b3, b4;

  b1 = bitmap_create ();
  b2 = bitmap_create ();
  b3 = bitmap_create ();
  b4 = bitmap_create ();
  status = bitmap_empty_p (b1);
  status &= bitmap_bit_count (b1) == 0;

  status &= bitmap_set_bit_p (b1, 1);
  status &= bitmap_set_bit_p (b1, 120);
  status &= !bitmap_set_bit_p (b1, 120);
  status &= !bitmap_empty_p (b1);
  status &= bitmap_bit_p (b1, 1);
  status &= bitmap_bit_p (b1, 120);
  status &= !bitmap_bit_p (b1, 42);

  status &= bitmap_clear_bit_p (b1, 120);
  status &= !bitmap_bit_p (b1, 120);
  status &= bitmap_set_bit_p (b1, 120);

  bitmap_copy (b2, b1);
  status &= bitmap_equal_p (b1, b2);
  status &= bitmap_intersect_p (b1, b2);
  status &= !bitmap_equal_p (b1, b3);
  status &= !bitmap_intersect_p (b1, b3);

  bitmap_clear (b2);
  status &= bitmap_empty_p (b2);
  status &= bitmap_bit_count (b2) == 0;

  bitmap_copy (b2, b1);
  status &= bitmap_equal_p (b1, b2);
  status &= bitmap_set_bit_p (b2, 1818);

  status &= bitmap_set_bit_p (b3, 555);
  status &= bitmap_set_bit_p (b3, 120);
  status &= bitmap_set_bit_p (b3, 42);
  status &= !bitmap_empty_p (b3);
  status &= bitmap_bit_count (b3) == 3;
  status &= bitmap_bit_p (b3, 555);
  status &= bitmap_bit_p (b3, 120);
  status &= bitmap_bit_p (b3, 42);

  status &= bitmap_and (b4, b1, b2);
  status &= bitmap_equal_p (b4, b1);

  status &= bitmap_ior (b4, b1, b2);
  status &= bitmap_equal_p (b4, b2);

  status &= bitmap_and_compl (b4, b2, b1);
  status &= bitmap_bit_p (b4, 1818);
  status &= bitmap_bit_count (b4) == 1;

  status &= bitmap_and_compl (b4, b1, b2);
  status &= bitmap_bit_count (b4) == 0;

  status &= bitmap_ior_and (b4, b1, b2, b3);
  status &= bitmap_bit_p (b4, 1);
  status &= bitmap_bit_p (b4, 120);
  status &= bitmap_bit_count (b4) == 2;

  status &= bitmap_ior_and (b4, b3, b1, b2);
  status &= bitmap_bit_p (b4, 1);
  status &= bitmap_bit_p (b4, 555);
  status &= bitmap_bit_p (b4, 42);
  status &= bitmap_bit_p (b4, 120);
  status &= bitmap_bit_count (b4) == 4;

  status &= bitmap_ior_and_compl (b4, b1, b2, b3);
  status &= bitmap_bit_p (b4, 1);
  status &= bitmap_bit_p (b4, 1818);
  status &= bitmap_bit_p (b4, 120);
  status &= bitmap_bit_count (b4) == 3;

  status &= bitmap_ior_and_compl (b3, b1, b2, b3);
  status &= bitmap_bit_p (b3, 1);
  status &= bitmap_bit_p (b3, 1818);
  status &= bitmap_bit_p (b3, 120);
  status &= bitmap_bit_count (b3) == 3;

  fprintf (stderr, status ? "DLIST OK\n" : "DLIST FAILURE!\n");
  bitmap_destroy (b1);
  bitmap_destroy (b2);
  bitmap_destroy (b3);
  bitmap_destroy (b4);
  return !status;
}
