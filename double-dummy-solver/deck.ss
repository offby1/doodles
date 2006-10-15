#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module deck mzscheme
(require
 "card.ss"
 "hand.ss"
 (only (lib "list.ss") sort)
 (only (lib "1.ss" "srfi")
       circular-list
       iota
       ))
(provide (all-defined))
(define *deck*
  (let loop ((suits *suits*)
             (result '()))
    (if (null? suits)
        (sort result (lambda (a b)
                       (< (card-rank a)
                          (card-rank b))))
      (loop (cdr suits)
            (append
             (let loop ((ranks (iota *num-ranks* 2))
                        (result '()))
               (if (null? ranks)
                   result
                 (loop (cdr ranks)
                       (cons (make-card (car suits)
                                        (car ranks))
                             result))))
             result)))))

(define (deal! deck hands)
  (let loop ((d deck)
             (hs (apply circular-list hands)))
    (unless (null? d)
      (let ((victim (car hs)))
        (add-card! victim (car d)))
      (loop (cdr d)
            (cdr hs)))))
)