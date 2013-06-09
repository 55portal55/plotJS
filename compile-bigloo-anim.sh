#!/bin/sh

# build plot.scm from scheme source code for animation application

cat src/module.scm src/datastart.scm data src/dataend.scm \
  src/plot.scm src/displaysize.scm src/math.scm expt.scm \
  src/jitter.scm  src/complex.scm src/read.scm \
  src/color.scm src/domain-coloring.scm \
  src/eval.scm src/plotter.scm src/main.scm src/callmainanim.scm \
  > x.scm

bigloo -o x x.scm
