#lang racket

(require "q.rkt"
         unstable/debug)

(provide b-f-traverse)
(define (b-f-traverse init generate-neighbors)
  (let loop ([node-to-predecessor (make-immutable-hash (list (cons init '())))]
             [q (enqueue (make-queue) init)])
    (if (queue-empty? q)
        node-to-predecessor
        (let-values ([(item q) (dequeue q)])
          (define (visited? thing) (hash-has-key? node-to-predecessor thing))
          (let ([neighbors (filter (compose not visited?) (generate-neighbors item))
                           ])
            (call-with-values
                (thunk
                 (for/fold ([node-to-predecessor node-to-predecessor]
                            [q q])
                     ([n neighbors])
                     (values
                      (hash-update
                       node-to-predecessor
                       n
                       (lambda (existing)
                         (cons existing  (hash-ref node-to-predecessor item))) item)

                      (enqueue q n))
                   ))
              loop))))))

(sort
 (hash-map
  (b-f-traverse
   'fred
   (lambda (n)
     (case n
       ((fred)
        (list 'tim 'harmony))
       ((tim mary)
        (list 'zed))
       ((zed clem)
        (list 'harmony))
       (else
        (list)))))
  (lambda (dest trail)
    (reverse (cons dest trail))))
 <
 #:key length)
