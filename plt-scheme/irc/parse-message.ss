#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module parse-message mzscheme
(require
         (only (planet "port.ss" ("schematics" "port.plt" 1 0)) port->string))

(provide parse-message)
(define (grab-word ip)
  (let loop ((chars '()))
    (let ((ch (read-char ip)))
      (cond
       ((or (eof-object? ch)
            (char=? #\space ch))

        ;;  consume any remaining spaces
        (let loop ((ch (peek-char ip)))
          (when (and (not (eof-object? ch))
                     (char=? #\space ch))
            (read-char ip)))

        (apply string (reverse chars)))
       (else
        (loop (cons ch chars)))))))

(define (parse-message str)
  (let ((prefix #f)
        (command #f)
        (params #f))
    (let ((ip (open-input-string str)))
      (when (equal? #\: (peek-char ip))
        (read-char ip)
        (set! prefix (grab-word ip)))
      (set! command (grab-word ip))
      (set! params (port->string ip))
      (values prefix command params)))
  )
)