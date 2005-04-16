(module calls mzscheme
  (require (lib "list.ss" "srfi" "1"))

  (require (lib "compat.ss")            ;for "sort", at least
           )

  (provide all-legal-calls-I-could-make-now
           auction-is-completed)
 
  (define (all-legal-calls-I-could-make-now auction-so-far)
    ;; BUGBUG -- doesn't deal with doubles.
    (let* ((last-call   (last auction-so-far))
           )
      (list-tail *all-bids* (+ 1 (bid->number last-call)))))
  
  
  (define level car)
  (define (denomination b)
    (hash-table-get *denoms* (cadr b)))
  
  (define *denoms* (make-immutable-hash-table '((clubs    . 0)
                                                (diamonds . 1)
                                                (hearts   . 2)
                                                (spades   . 3)
                                                (notrump  . 4))))
  
  (define *levels*  (map (lambda (x) (+ 1 x)) (iota 7)))
  
  ;; excludes doubles and redoubles
  (define *all-bids*
    (apply
     append
     (map
      (lambda (d)
        (map (lambda (l)
               (list d l))
             (map car (sort (lambda pairs
                              (apply < (map cdr pairs)))
                            (hash-table-map *denoms* cons)))))
      *levels*)))
  
  (define (bid->number b)
    (+ (* (hash-table-count *denoms*) (- (level b) 1))
       (denomination b)))
  
  (define (bid> . bids)
    (apply > (map bid->number bids)))

  (define (auction-is-completed a)
    (and (< 3 (length a))
         (let ((last-three-calls (take-right a 3)))
           (and (every (lambda (c)
                         (eq? c 'pass)) last-three-calls)))))
  
  )

;; Local Variables:
;; mode: scheme
;; End:
