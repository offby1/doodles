(module calls mzscheme

  ;; it'd be clearer to simply require  (lib "1.ss" "srfi"), but ... that causes this error:
;;; module: identifier already imported (from a different source) at: reverse! in: (require (lib "1.ss" "srfi"))
  
  (require (lib "trace.ss")
           (lib "list.ss" "srfi" "1")
           (lib "compat.ss")            ;for "sort", at least
           "slow.ss"
           )

  (provide all-legal-calls-I-could-make-now
           auction-is-completed
           predict-scores)
 
  (define bid? pair?)
  
  (define (all-legal-calls-I-could-make-now auction-so-far)
    ;; BUGBUG -- doesn't deal with doubles.
    (let* ((last-call (last (filter bid? auction-so-far)))
           )
      (cons 'pass (list-tail *all-bids* (+ 1 (bid->number last-call))))))
  
  
  (define level car)
  (define (denomination b)
    (hash-table-get *denoms* (cadr b)))
  
  (define *denoms* (make-immutable-hash-table '((clubs    . 0)
                                                (diamonds . 1)
                                                (hearts   . 2)
                                                (spades   . 3)
                                                (notrump  . 4)
                                                )))
  
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
  
  (define (extend-auction a c)
    (append a (list c)))
  
  (define (extract-incomplete-knowledge . args)
    "duh?")
  
  ;; this of course is a stub.
  (define (most-likely-score . args)
    (* 50 (- 10 (remainder (eq-hash-code args) 20))))

  (define (predict-scores auction-so-far)
    (cond
     ((auction-is-completed auction-so-far)
      (list
       (cons

        (most-likely-score (extract-incomplete-knowledge auction-so-far)
                           auction-so-far)
        auction-so-far)))
     
     (else
      (apply
       append
       (partial-map
        1
        (lambda (one-possible-call)
          (let ((extended (extend-auction auction-so-far one-possible-call)))
            (predict-scores extended)))

        ;; possible optimization: rather than considering _every_
        ;; legal call I could make, consider only the "first" few,
        ;; where "first" means "lowest level".

        (all-legal-calls-I-could-make-now auction-so-far)
        )))
     )
    )

  (trace predict-scores)

  )
