#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module card mzscheme
(print-struct #t)
(require (lib "assert.ss" "offby1"))
(provide make-card card? card-suit)

;; I'm using make-struct-type rather than define-struct here, simply
;; so that I can provide a guard procedure.
(define-values (struct:card make-card card? card-ref card-set!)
  (make-struct-type 'card #f 2 0 #f null #f #f '(0 1)
                    (lambda (suit rank name)
                      (unless (memq suit '(clubs diamonds hearts spades))
                        (error (string->symbol (format "make-~a" name))
                               "first field must be a suit"))
                      (unless (and (exact? rank)
                                   (integer? rank)
                                   (<= 2 rank 14))
                        (error (string->symbol (format "make-~a" name))
                               "second field must be a number 'twixt 2 and 14"))
                      (values suit rank))))

(define (card-suit c) (card-ref c 0))

)