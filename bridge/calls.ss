(module calls mzscheme
  (require (lib "compat.ss")            ;for "sort", at least
           (rename (lib "1.ss" "srfi") iota iota)
           (planet "test.ss" ("schematics" "schemeunit.plt" 1))))
  
  (provide all-legal-calls-I-could-make-now)

  (define (last l)
    (list-tail l (- (length l) 1)))

  (define (all-legal-calls-I-could-make-now auction-so-far)
    ;; BUGBUG -- doesn't deal with doubles.
    (let ((level (level auction-so-far))
          (denomination (denomination auction-so-far)))
      (last auction-so-far)))
  

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
  )