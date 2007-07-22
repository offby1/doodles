#!/bin/sh -x

mzscheme -M errortrace -qr ./coverage.ss --test-mode --channel '#snorkulous' --channel '#bot-testing-party' "$@"

