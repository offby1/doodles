#lang racket

(struct queue (forward reverse) #:prefab)

(provide make-queue)
(define (make-queue)
  (queue '() '()))

(provide enqueue)
(define (enqueue q item)
  (queue (queue-forward q)
         (cons item (queue-reverse q))))

(provide dequeue)
(define (dequeue q)
  (if (null? (queue-forward q))
      (dequeue (queue (reverse (queue-reverse q))
                      '()))
      (values (car (queue-forward q))
              (queue (cdr (queue-forward q))
                     (queue-reverse q)))))

(provide queue-empty?)
(define (queue-empty? q)
  (and (null? (queue-forward q))
       (null? (queue-reverse q))))
