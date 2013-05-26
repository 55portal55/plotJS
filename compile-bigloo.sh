#!/bin/sh

# build plot.scm from scheme source code

# compile the application using bigloo scheme. It is straightforward to
# modify the script to handle whichever flavor of scheme you wish to use.

cat module.scm src/plot.scm src/displaysize.scm src/math.scm src/complex.scm \
  src/read.scm src/color.scm src/domain-coloring.scm \
  src/eval.scm src/plotter.scm src/main.scm src/callmain.scm \
  > x.scm

bigloo -o x x.scm
