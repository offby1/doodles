#!/bin/bash

here=$(cd $(dirname $0) && pwd)
python3 -m doctest "$here/enigma.py"

inf=/etc/passwd

outf=$(mktemp)
trap 'rm -rf $outf' EXIT

go()
{
    python3 "$here/enigma.py"
}

log ()
{
    echo -e "$@" > /dev/stderr
}

head -10 "$inf" | go  | go > "$outf"
log "This should be the first ten lines of ${inf}:"
cat "$outf"
log "\n======================="

log "Encrypting /usr/share/dict/words, just to see how long it takes ..."
time go < /usr/share/dict/words | go > /dev/null
