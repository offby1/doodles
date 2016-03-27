#!/bin/bash

here=$(cd $(dirname $0) && pwd)
inf=/etc/passwd

outf=$(mktemp)
trap 'rm -rf $outf' EXIT

go()
{
    python3 "$here/enigma.py"
}

log ()
{
    echo "$@" > /dev/stderr
}

go < "$inf" | go > "$outf"
log "This should be the first two lines of ${inf}:"
head -2 "$outf"
log "======================="
cmp --quiet "$inf" "$outf" || exit 1

log "Encrypting /usr/share/dict/words, just to see how long it takes ..."
time go < /usr/share/dict/words | go > /dev/null

