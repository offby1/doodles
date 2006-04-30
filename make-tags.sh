#!/bin/sh

cd $(dirname $0)
sfind.pl . -type f \( -name '*.scm' -o -name '*.ss' \) -print0 | xargs -0 etags
