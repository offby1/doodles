#!/bin/sh

set -e

# This worked on a Debian `testing' system as of 19 May 2004.

# Some observations about the relative merits of the different Lisps:

# clisp seems to be the fastest by far.

# sbcl's and cmu's compiler outputs are very informative; I bet if I
# fixed everything they were whining about, the program would run
# faster.  clisp's output is quite terse; I wonder if I can give some
# switch to make it whine louder.

for cmd in "clisp -q -i" "lisp -load" "sbcl --load"
  do 
  time $cmd profile.l
  echo 
done
