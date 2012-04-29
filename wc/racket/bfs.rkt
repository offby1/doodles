#lang racket

(require "q.rkt"
         unstable/debug)

(provide bfs)
(define (bfs init generate-neighbors)
  (let loop ([node-to-distance (make-immutable-hash (list (cons init 0)))]
             [q (enqueue (make-queue) init)])
    (if (queue-empty? q)
        (values node-to-distance q)
        (let-values ([(item q) (dequeue q)])
          (define (visited? thing) (hash-ref node-to-distance thing #f))
          (let ([neighbors (filter (compose not visited?) (set->list (generate-neighbors item)))
                           ])
            (call-with-values
                (thunk
                 (for/fold ([node-to-distance node-to-distance]
                            [q q])
                     ([n neighbors])
                     (values
                      (let ([default (add1 (hash-ref node-to-distance item))])
                        (hash-update
                         node-to-distance
                         n
                         (lambda (original-distance)
                           (min original-distance default))
                         default))
                      (enqueue q n))
                   ))
              loop))))))

(bfs
 'fred
 (lambda (n)
   (case n
     ((fred)
      (set 'tim 'harmony))
     ((tim mary)
      (set 'zed))
     ((zed clem)
      (set 'harmony))
     (else
      (set)))))
