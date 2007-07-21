#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require (lib "errortrace.ss" "errortrace")
         (lib "etc.ss"))

(coverage-counts-enabled #t)
(execute-counts-enabled #t)
(profile-paths-enabled #t)
(profiling-enabled #t)
(profiling-record-enabled #t)

(require "bot.ss")

(define *file-of-interest* (simplify-path (build-path (this-expression-source-directory) "bot.ss")))
(define *profile-output-fn* "profile-data.txt")
(with-output-to-file *profile-output-fn*
  (lambda ()
    (printf "-*-fundamental-*-~%")      ; for emacs
    (annotate-covered-file  *file-of-interest* #t)
    (fprintf (current-error-port)
             "Spewed profile stuff to ~s~%" *profile-output-fn*))

  'truncate/replace)
