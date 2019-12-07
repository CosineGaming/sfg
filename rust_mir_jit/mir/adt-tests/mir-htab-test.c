#include "mir-htab.h"

DEF_HTAB (int);
static unsigned hash (int el) { return el; }
static int eq (int el1, int el2) { return el1 == el2; }
static int sum;
static void f (int i) { sum += i; }

int main (void) {
  int i, collisions, iter, tab_el, status = 1;
  HTAB (int) * htab;

  HTAB_CREATE_WITH_FREE_FUNC (int, htab, 4, hash, eq, f);
  status &= HTAB_ELS_NUM (int, htab) == 0;
  for (iter = 0; iter < 10; iter++) {
    for (i = 0; i < 100; i++) {
      status &= !HTAB_DO (int, htab, i, HTAB_INSERT, tab_el);
      status &= tab_el == i;
      status &= HTAB_ELS_NUM (int, htab) == i + 1;
    }
    sum = 0;
    HTAB_CLEAR (int, htab);
    status &= sum == 4950;
    status &= HTAB_ELS_NUM (int, htab) == 0;
    for (i = 0; i < 100; i++) {
      status &= !HTAB_DO (int, htab, i, HTAB_INSERT, tab_el);
      status &= tab_el == i;
      status &= HTAB_ELS_NUM (int, htab) == i + 1;
    }
    for (i = 0; i < 100; i++) {
      status &= HTAB_DO (int, htab, i, HTAB_FIND, tab_el);
      status &= tab_el == i;
    }
    for (i = 0; i < 100; i++) {
      status &= HTAB_DO (int, htab, i, HTAB_REPLACE, tab_el);
      status &= tab_el == i;
      status &= HTAB_ELS_NUM (int, htab) == 100;
    }
    status &= sum == 9900;
    for (i = 0; i < 100; i++) {
      tab_el = 42;
      status &= HTAB_DO (int, htab, i, HTAB_DELETE, tab_el);
      status &= tab_el == 42;
      status &= HTAB_ELS_NUM (int, htab) == 100 - i - 1;
    }
    status &= sum == 14850;
  }
  collisions = HTAB_COLLISIONS (int, htab);
  HTAB_DESTROY (int, htab);
  status &= sum == 14850;
  fprintf (stderr, status ? "HTAB OK" : "HTAB FAILURE!");
  fprintf (stderr, ": collisions = %d\n", collisions);
  return !status;
}
