#!/bin/bash

here=$(cd $(dirname $0) && pwd)
inf=/etc/passwd

outf=$(mktemp)
trap 'rm -rf $outf' EXIT

go()
{
    python3 "$here/enigma.py"
}

go < "$inf" | go > "$outf"
head -2 "$outf"
cmp --quiet "$inf" "$outf" || exit 1

time go < /usr/share/dict/words | go > /dev/null

