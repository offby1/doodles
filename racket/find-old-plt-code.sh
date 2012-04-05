#!/bin/sh

(locate .ss; locate .scm) |                     \
    egrep /erich/ |                             \
    fgrep -v /.plt-scheme/ |                    \
    fgrep -v /squire/ |                         \
    fgrep -v /coverage/ |                       \
    xargs fgrep -l '(module ' |                 \
    xargs ls -lS |                              \
    head
