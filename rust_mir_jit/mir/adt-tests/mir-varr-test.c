#include "mir-varr.h"

DEF_VARR (int);
int main (void) {
  int status;
  VARR (int) * test;
  int arr[] = {1, 2, 3};

  VARR_CREATE (int, test, 0);
  status = VARR_LENGTH (int, test) == 0;
  VARR_PUSH (int, test, 42);
  status &= VARR_LAST (int, test) == 42;
  VARR_PUSH (int, test, 8);
  status &= VARR_LAST (int, test) == 8;
  status &= VARR_SET (int, test, 1, 7) == 8;
  status &= VARR_GET (int, test, 1) == 7;
  VARR_EXPAND (int, test, 10);
  status &= VARR_LENGTH (int, test) == 2;
  status &= VARR_ADDR (int, test)[0] == 42 && VARR_ADDR (int, test)[1] == 7;
  VARR_PUSH_ARR (int, test, arr, 3);
  VARR_PUSH (int, test, 4);
  status &= (VARR_ADDR (int, test)[0] == 42 && VARR_ADDR (int, test)[1] == 7
             && VARR_ADDR (int, test)[2] == 1 && VARR_ADDR (int, test)[3] == 2
             && VARR_ADDR (int, test)[4] == 3 && VARR_ADDR (int, test)[5] == 4
             && VARR_LENGTH (int, test) == 6);
  VARR_TRUNC (int, test, 1);
  status &= VARR_LENGTH (int, test) == 1;
  status &= VARR_POP (int, test) == 42;
  VARR_TRUNC (int, test, 0);
  VARR_TAILOR (int, test, 10);
  status &= VARR_LENGTH (int, test) == 10;
  VARR_PUSH (int, test, 42);
  status &= VARR_ADDR (int, test)[10] == 42;
  VARR_TAILOR (int, test, 1);
  status &= VARR_LENGTH (int, test) == 1;
  VARR_DESTROY (int, test);
  fprintf (stderr, status ? "VARR OK\n" : "VARR FAILURE!\n");
  return !status;
}
