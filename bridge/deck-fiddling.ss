(module deck-fiddling mzscheme
  (require "constants.ss"
           "hand.ss"
           "deck.ss"
           "card.ss"
           (lib "list.ss" "srfi" "1")
           (lib "13.ss" "srfi")
           (only (lib "compat.ss") sort))
  
  ;; hand -> four-element list
  (define (shape h)
    (map (lambda (s)
           (length (filter (lambda (c) (eq? s (card-suit c)))
                           (hand->list h))))
         *suits*))
  
  (define (hand->string h)
    (map card->short-string (sort card< (hand->list h))))

  (define *coca-cola-hands* 0)

  (define *deals* 1000)
  
  (define shape-distribution #f)
  (define note-hand! #f)

  (let ()
    (define *shapes* (make-hash-table 'equal))

    (set! shape-distribution 
          (lambda ()
            (hash-table-map *shapes* (lambda (k v) (list v k)))))

    (let ((hands-seen 0))
      (set! note-hand!
            (lambda (h)
              (define (max-rank)
                (apply max (map card-rank (hand->list h))))
              (define (hash-table-increment! h k)
                (let ((v (hash-table-get h k (lambda () 0))))
                  (hash-table-put! h k (add1 v))))
              (hash-table-increment! *shapes* (sort > (shape h)))
              (set! hands-seen (add1 hands-seen))
              (when (<= (max-rank) 10)
                (set! *coca-cola-hands* (add1 *coca-cola-hands*)))))))
  
  (let loop ((d (shuffled-deck))
             (deals 0))
    (when (< deals *deals*)
      (for-each 
       (lambda (s)
         (note-hand! (holding d s)))
       *seats*)  
      (loop (shuffled-deck) (add1 deals))))

  (printf "After ~a deals~n~a: ~a~n" *deals* "Shape" "likelihood")
  (let* ((results (map (lambda (thing)
                         (list
                          (second thing)
                          (first thing)))
                       (sort (lambda (a b)
                               (> (car a)
                                  (car b))) (shape-distribution))))
         (num-hands (apply + (map second results))))
    (printf "~a~n"
            (string-join
             (map (lambda (p)
                    (format "~a: ~a"
                            (first p)
                            (exact->inexact (/ (second p) num-hands ))))
                  results)
             "\n"))
    (printf "Fraction of coca-cola hands (i.e., hands with no face cards): ~a~n"
            (exact->inexact (/ *coca-cola-hands* num-hands))))
  
  (newline)
  )
