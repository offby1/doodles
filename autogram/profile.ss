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

(require "s3.scm")
(printf "Hey Emacs, -*- coding:utf-8 -*- rocks!~%")
(for-each (lambda (datum)
            (apply
             (lambda (called milliseconds name source paths)
               (printf "time = ~a : no. = ~a : µs per call = ~a : ~a ~a~%"
                       milliseconds
                       called
                       (if (or (zero? called))
                           +inf.0
                         (exact->inexact
                          (/ (truncate (* 10000
                                          (/ milliseconds called)))
                             10)))
                       name
                       source)
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
                  (< (car a)
                     (car b)))))
