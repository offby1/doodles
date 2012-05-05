#!/bin/sh

here=$(cd $(dirname $0); pwd)

set -e

for file in $(find $here -maxdepth 1 -name '*.rkt' -perm +100)
do
    echo "$file ..."
    $file -t
    echo
done
