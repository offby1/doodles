#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require (lib "errortrace.ss" "errortrace"))

(profiling-enabled #t)
;;(profiling-record-enabled #t)

(coverage-counts-enabled #t)
(execute-counts-enabled  #t)
(require "bot.ss")

(printf "Coverage counts: ~s~%Execute counts: ~s~%"
        (get-coverage-counts)
        (get-execute-counts))
