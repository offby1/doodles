(module deck-fiddling mzscheme
  (require
           "card.ss"
           "constants.ss"
           "deck.ss"
           "hand.ss"
           "misc.ss"
           (lib "list.ss" "srfi" "1")
           (only (lib "13.ss" "srfi") string-join)
           (only (lib "compat.ss") sort))

  ;; hand -> four-element list
  (define (shape h)
    (map (lambda (s)
           (length (filter (lambda (c) (eq? s (card-suit c)))
                           (hand->list h))))
         *suits*))

  (define (high-card-points h)
    (apply + (map (lambda (c) (max 0 (- (card-rank c) 10)))
                  (hand->list h))))

  (define (hand->string h)
    (map card->short-string (sort card< (hand->list h))))

  (define *deals* 1000)

  (define shape-distribution #f)
  (define note-hand! #f)
  
  (define *points* (make-hash-table))
  
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
              (hash-table-increment! *shapes* (sort > (shape h)))
              (hash-table-increment! *points* (high-card-points h))
              (set! hands-seen (add1 hands-seen))))))

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

    (printf "Histogram of high-card points:~%")
    (for-each
     (lambda (p) (printf "~a: ~v~%" (first p) (exact->inexact (/ (second p) num-hands))))
     (sort (lambda (a b)
             (< (car a)
                (car b))) (hash-table-map *points* list))))

  (newline))
