(module deck-fiddling mzscheme
  (require "constants.ss"
           "hand.ss"
           "deck.ss"
           "card.ss"
           (lib "list.ss" "srfi" "1")
           (rename (lib "compat.ss") sort sort))
  

  ;; hand -> four-element list
  (define (shape h)
    (set! h (hand->list h))
    (map (lambda (s)
           (length (filter (lambda (c) (eq? s (card-suit c))) h))
           )
         *suits*)
    )
    
  (define (hand->string h)
    (map card->short-string (sort card< (hand->list h))))

  (define (hash-table-increment! h k)
    (let ((v (hash-table-get h k (lambda () 0))))
      (hash-table-put! h k (add1 v))))

  (define *shapes* (make-hash-table 'equal))
  (define *coca-cola-hands* 0)

  (define *passes* 10000)
  
  (define (shape-distribution)
    (hash-table-map *shapes* (lambda (k v) (list v k))))

  (define (note-hand! h)
    (define (max-rank)
      (apply max (map card-rank (hand->list h))))
    (hash-table-increment! *shapes* (sort > (shape h)))
    (when (<= (max-rank) 10)
      (set! *coca-cola-hands* (add1 *coca-cola-hands*))))

  (let loop ((d (shuffled-deck))
             (passes 0))
    (when (< passes *passes*)
      (for-each 
       (lambda (s)
         (note-hand! (holding d s)))
       *seats*)  
      (loop (shuffled-deck) (add1 passes))))

  (printf "After ~a passes~n~a: ~a~n" *passes* "Shape" "likelihood")
  (for-each (lambda (thing)
              (printf "~a: ~a~n"
                      (cadr thing)
                      (exact->inexact (/ (car thing) (* (length *seats*) *passes*)))))
            (sort (lambda (a b)
                   (> (car a)
                      (car b))) (shape-distribution)))
  (printf "Fraction of coca-cola hands (i.e., hands with no face cards): ~a~n" (exact->inexact (/ *coca-cola-hands* *passes*)))
  (newline)
  )
