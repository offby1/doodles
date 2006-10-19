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
       take
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

(define (deal deck hands)
  (let loop ((d deck)
             (lol (map cards hands)))
    (if (null? d)
        (map (lambda (cards h)
               (make-hand cards (seat h)))
             (take lol (length hands))
             hands)
      (let ((this-hand (cons (car d) (car lol))))
        (loop (cdr d)
              (append (cdr lol)
                      (list this-hand)))))))
)