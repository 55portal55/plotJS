#!/bin/sh

# script creates a plotJS animation. This produces the youtube animation
# at http://youtu.be/TouRFNFL1k0. The softwrae can be adapted to produce any
# desired animation.

# first compile some c commands

gcc -O3 -o seq src/seq.c
gcc -O3 -o antialias src/antialias.c
gcc -O3 -DUNIX -o data2bmp src/data2bmp.c

FIRST_FRAME=0
LAST_FRAME=479
N_FRAMES=480

mkdir -p frames

for frame in `./seq $FIRST_FRAME $LAST_FRAME`; do
  echo $frame
  echo "1920 1920 $frame $N_FRAMES" >data
  ./compile-bigloo-anim.sh
  # antialias a 4x4 grid of pixels down to a single pixel
  ./x <data | ./antialias | ./antialias | ./data2bmp x.bmp
  if [ $frame -lt 10 ]; then
    file=000$frame
  elif [ $frame -lt 100 ]; then
    file=00$frame
  elif [ $frame -lt 1000 ]; then
    file=0$frame
  else
    file=$frame
  fi
  mv x.bmp frames/$file.bmp
done
