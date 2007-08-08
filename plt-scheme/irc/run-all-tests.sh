#!/bin/sh

# This should,of course, be scheme, not shell, but I'm in a hurry.

set -e

./bot-tests.ss
./parse.ss
./tinyurl-task.ss
./tinyurl.ss
