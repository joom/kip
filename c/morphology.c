#include "morphology.h"
#include <stdlib.h>
#include <stdbool.h>

#define LIMIT 50

char **ups(struct fsm* fsm, char *s)
{

  char **arr = (char **) calloc(LIMIT, LIMIT * sizeof(char *));
  struct apply_handle *h = apply_init(fsm);
  bool once = false;
  for(int i = 0; i < LIMIT; i++)
  {
    char *result = apply_up(h, once ? NULL : s);
    if (result == NULL) {
      break;
    } else {
      once = once || true;
      arr[i] = strdup(result);
    }
  }
  apply_clear(h);
  return arr;
}
