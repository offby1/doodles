(require 'random)
(require 'sort)

(let ()
  (define (shuffle-list l)
    (map
     car
     (sort
      (map
       (lambda (entry)
         (cons entry (random most-positive-fixnum)))
       l)
      (lambda (entry1 entry2)
        (< (cdr entry1)
           (cdr entry2))))))

  (define (shuffle-vector v)
    (list->vector
     (shuffle-list
      (vector->list v))))

  (define (shuffle-string s)
    (list->string
     (shuffle-list
      (string->list s))))

     ;; Simpler but much slower.  In fact, it seems possible that it might
     ;; never complete.

  (define (shuffle l)
    (sort
     l
     (lambda (entry1 entry2)
       (positive? (random 2)))))


     (shuffle-list (list 0 1 2 3 4 5 6 7 8 9)))
