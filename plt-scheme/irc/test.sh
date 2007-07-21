#!/bin/sh

# I wonder if GNU "expect" would make this test easier to write.

# I wonder if I should make a stub IRC server instead.

./profile.ss --host localhost --timeout 10 --channel '#emacs' &
sleep 5
cat<<EOF   | nc localhost  6667 -q 5
NICK offby2
USER erich debian irc.freenode.org :Eric Hanchrow
NAMES
JOIN #emacs
PRIVMSG rudybot :what up
PRIVMSG rudybot VERSION
PRIVMSG rudybot :OK, that's all.
QUIT done submitting test data
EOF

wait
