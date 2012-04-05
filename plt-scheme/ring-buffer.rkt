#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require rackunit)

(struct ring-buffer (max-size current-size data) #:transparent)

(define (make [size 5])
  (ring-buffer size 0 '()))

(define (add rb datum)
  (let ([new-size (min (ring-buffer-max-size rb)
                       (add1 (ring-buffer-current-size rb)))]
        [new-data (cons datum
                        (if (= (ring-buffer-current-size rb) (ring-buffer-max-size rb))
                            (drop-right (ring-buffer-data rb) 1)
                            (ring-buffer-data rb)))])
    (ring-buffer
     (ring-buffer-max-size rb)
     new-size
     new-data
     )))

(define (->list rb)
  (ring-buffer-data rb))

(let ([rb (make 2)])
  (check-equal? '() (->list rb))

  (set! rb (add rb 'frotz))
  (check-equal? (->list rb) '(frotz))

  (set! rb (add rb 'plotz))
  (check-equal? (->list rb) '(plotz frotz))

  (set! rb (add rb 'snork))
  (check-equal? (->list rb) '(snork plotz))
  )
