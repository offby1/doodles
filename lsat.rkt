#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require racket/pretty)

;; http://www.thebigquestions.com/2010/11/09/law-school-admissions-test/

(define (shuffle-list l)
  (sort
   l
   <
   #:key (lambda (_) (random))
   #:cache-keys? #t))

(define (list->histogram l)
  (hash-map
   (for/fold ([rv (make-immutable-hash '())])
       ([elt (in-list l)])
       (hash-update rv elt add1 0))
   cons))

(define urn->list values)

(define (make-urn reds blacks)
  (append (build-list reds (lambda (_) 'red))
          (build-list blacks (lambda (_) 'black))))

(define (draw-from-urn number-of-balls u)
  (take (shuffle-list (urn->list u)) number-of-balls))

(define (red? thing)
  (eq? thing 'red))

(define (redness thing)
  (if (red? thing) 1 0))

(define (blackness thing)
  (if (red? thing) 0 1))

(define (summarize-experiment list-of-balls)
  (call-with-values
      (lambda ()
        (for/fold ([red 0]
                   [black 0])
            ([ball list-of-balls])
            (values (+ red   (redness   ball))
                    (+ black (blackness ball)))))
    (lambda (reds blacks)
      `((reds . ,reds)
        (blacks . ,blacks)))))

(define *70-red* (make-urn 70 30))

(provide main)
(define (main . args)
  (pretty-display
   (sort (list->histogram
          (for/list ([_ (in-range 10000)])
            (summarize-experiment (draw-from-urn 12 *70-red*))))
         < #:key cdr)))

#|
 When I run the above, I see output that includes lines like these:

 (((reds . 4) (blacks . 8)) . 40)
 ...
 (((reds . 8) (blacks . 4)) . 2456)

That says to me that pulling 8 of the majority and 4 of the minority
is 2456/40 (about 60) times more likely than the reverse. -- about a
98% chance.

|#