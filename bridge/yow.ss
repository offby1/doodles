#! /bin/sh
#|
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require  "calls.ss")
(predict-scores '((6 clubs)))
