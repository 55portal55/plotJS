#include <stdio.h>

int main(int argc, char* argv[]) {
  int first, last, i;
  first = atoi(argv[1]);
  last = atoi(argv[2]);
  for (i = first; i < last; i++)
    printf("%d\n", i);
  printf("%d", last);
}
