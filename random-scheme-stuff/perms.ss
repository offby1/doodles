#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module perms mzscheme
(require (only (lib "1.ss" "srfi") circular-list))

;; list the Unix octal permissions that people actually use.

;; the Unix octal permissions are simply three octal digits, which
;; means in theory there are 8^3 possibilities.  But in reality, only
;; a few of those actually get used.  They are:

;; * those for which the digits are non-increasing -- e.g., 654 is
;; non-increasing, so it might be used; but 345 is increasing, so it
;; will never be used;

;; * those for which the digits are either 0, 4, 5, 6, or 7.  1, 2,
;; and 3 are not used in practice.  (I'm ignoring the setuid and
;; setgid bits here.)

;; So in effect, they're not octal, but rather base 5.

(define *digits* '(0 4 5 6 7))

;; TODO -- generalize this, obviously, so that it can have any number
;; of digits -- not just three.
(let loop ((high   (apply circular-list *digits*))
           (middle (apply circular-list *digits*))
           (low    (apply circular-list *digits*))
           (result '()))
  (if (and (pair? result)
           (equal? '(7 7 7)
                   (car result)))
      (reverse result)
    (loop (if (and (= 7 (car middle))
                   (= 7 (car low)))
              (cdr high)
            high)
          (if (= 7 (car low))
              (cdr middle)
            middle)
          (cdr low)
          (if (>= (car high)
                  (car middle)
                  (car low))
              (cons (list (car high)
                          (car middle)
                          (car low))
                    result)
            result)))
  )

)