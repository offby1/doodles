#!/usr/local/bin/guile -s
!#

;; Eats the output of the program `popularity-contest', and converts
;; the timestamps (which are just big numbers) into human-readable
;; date-and-time strings.

;; This is really a Perl script trapped inside a Scheme program.

;; A typical input line looks like this:
;;
;;  956960701 955118882 grep /bin/egrep

(require 'split)
(require 'pretty-print)

(let ()

  (define (convert-timestamps one-line)
    (define (secs-to-string s) (strftime "%c %Z" (localtime (string->number s))))
    (let ((fields (split one-line " ")))
      (if (memq (length fields) '(4 5))
          (let ((first-time (car fields))
                (second-time (cadr fields)))
            (cons (secs-to-string first-time)
                  (cons (secs-to-string second-time)
                        (cddr fields))))
        one-line)))

  (with-output-to-file "/tmp/pop2"
    (lambda ()
      (pretty-print
       (with-input-from-file "/var/log/popularity-contest"
         (lambda ()
           (let loop ((one-line (read-line))
                      (result '()))
             (if (not (eof-object? one-line))
                 (loop (read-line)
                       (cons (convert-timestamps one-line) result))
               (reverse result)))))))))