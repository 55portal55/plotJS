#include <stdio.h>
#include <stdlib.h>

// usage: bmp2data x.bmp | antialias data2bmp y.bmp

int antialias(int value1, int value2, int value3, int value4) {
  int r1 = value1 & 0xff;
  int r2 = value2 & 0xff;
  int r3 = value3 & 0xff;
  int r4 = value4 & 0xff;
  int g1 = (value1 & 0xff00) >> 8;
  int g2 = (value2 & 0xff00) >> 8;
  int g3 = (value3 & 0xff00) >> 8;
  int g4 = (value4 & 0xff00) >> 8;
  int b1 = (value1 & 0xff0000) >> 16;
  int b2 = (value2 & 0xff0000) >> 16;
  int b3 = (value3 & 0xff0000) >> 16;
  int b4 = (value4 & 0xff0000) >> 16;
  int r = (int)(((double)r1 + (double)r2 + (double)r3 + (double)r4) / 4.0);
  int g = (int)(((double)g1 + (double)g2 + (double)g3 + (double)g4) / 4.0);
  int b = (int)(((double)b1 + (double)b2 + (double)b3 + (double)b4) / 4.0);

  return r | (g << 8) | (b << 16);
}

int main(int argc, char *argv[]) {
  int width, height;
  int i, j;
  int value;
  int row[2][16000]; // ???? kludged size 16000

  scanf("%d", &width);
  printf("%d\n", width / 2);
  scanf("%d", &height);
  printf("%d\n", height / 2);

  for (i = 0; i < height; i += 2) {
    for (j = 0; j < width; j++) {
      scanf("%d", &value);
      row[0][j] = value;
    }
    for (j = 0; j < width; j++) {
      scanf("%d", &value);
      row[1][j] = value;
    }
    for (j = 0; j < width; j += 2) {
      printf("%d\n", 
        antialias(row[0][j], row[0][j + 1], row[1][j], row[1][j + 1]));
    }
  }
}
