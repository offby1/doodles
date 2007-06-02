#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qr "$0" ${1+"$@"}
|#

(require (lib "errortrace.ss" "errortrace")
         (only (lib "list.ss") sort))

(profiling-enabled #t)
(profiling-record-enabled #t)
(execute-counts-enabled #t)
(profile-paths-enabled #t)

(require "autogram.ss")
(printf "Hey Emacs, -*- coding:utf-8 -*- rocks!~%")
(for-each (lambda (datum)
            ;; datum contains, in order:

            ;;   * the number of times the procedure was called;

            ;;   * the number of milliseconds of process time consumed by the
            ;;     procedure;

            ;;   * the inferred name or #f of the procedure;

            ;;   * the syntax source of the procedure; and

            ;;   * a list of unique call paths recorded while `profile-paths-enabled'
            ;;     is set to #t.  Each call path is a pair of a count (the number of
            ;;     times the path occurred) and a list containing two-element lists;
            ;;     each two-element list contains the calling procedure's name or
            ;;     source expression and the calling procedure's source file or #f.

            (apply
             (lambda (called milliseconds name source paths)
               ;;(error (syntax-property-symbol-keys source))
               (printf "~s (~a) : time = ~a : no. = ~a : µs per call = ~a~%"
                       source
                       name
                       milliseconds
                       called
                       (if (or (zero? called))
                           +inf.0
                         (exact->inexact
                          (/ (truncate (* 10000
                                          (/ milliseconds called)))
                             10)))
                       )
               (for-each (lambda (path)
                           (printf "   ~a~%" (car path))
                           (for-each (lambda (location)
                                       (printf "      ~a~%" location))
                                     (cdr path)))
                         (sort paths (lambda (a b)
                                       (> (car a)
                                          (car b))))))
             datum))
          (sort (get-profile-results)
                (lambda (a b)
                  ;; sort by time
                  (< (list-ref a 1)
                     (list-ref b 1)))))
