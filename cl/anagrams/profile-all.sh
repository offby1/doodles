#!/bin/sh

set -e

# This worked on a Debian `testing' system as of 19 May 2004.

# Some observations about the relative merits of the different Lisps:

# sbcl takes *six times longer* to read the dictionary than cmu, and
# clisp is even faster.  I assume this has something to do with the
# implementation of hash tables.

# clisp's profiling output looks to be the least useful; cmu's and
# sbcl's are pretty much the same.  And yet `slime' somehow manages to
# extract useful info from clisp (by using a file called
# metering.lisp).  I must figure out how to use that file outside of
# slime.

# sbcl's and cmu's compiler outputs are very informative; I bet if I
# fixed everything they were whining about, the program would run
# faster.  clisp's output is quite terse; I wonder if I can give some
# switch to make it whine louder.

for cmd in "clisp -q -i" "lisp -load" "sbcl --load"
  do 
  time $cmd profile.l
  echo 
done
