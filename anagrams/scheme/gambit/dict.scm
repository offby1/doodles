(define *big-ol-hash-table* #f)

(define (wordlist->hash fn)
  (with-input-from-file fn
    (lambda ()
      (let ((dict (make-hash-table 'equal)))
        (fprintf status-port  "Reading dictionary ... ")
        (let loop ((word  (read-line))
                   (words-read 0))
          (if (eof-object? word)
              (fprintf status-port  "Reading dictionary ... done.")
            (begin
              (if (word-acceptable? word)
                  (adjoin-word dict (srfi-13-string-downcase word)))
              (if (zero? (remainder words-read 1000))
                  (fprintf status-port  (format "Reading dictionary ... ~a words ..." words-read)))
              (loop (read-line)
                    (+ 1 words-read)))
            ))
        dict)
      )))

(define *dictionary* #f)

(define (adjoin-word dict word)
  (let* ((this-bag (bag word))
         (probe (hash-table-get dict this-bag (lambda () #f))))
    (cond
     ((not probe)
      (hash-table-put! dict this-bag (list-quicksort (list word) string<?)))
     ((not (member word probe))
      (hash-table-put! dict this-bag (cons word probe)))
     )))

(define (word-acceptable? word)
  (define (string-contains-matching-char? char-pred s)
    (with-input-from-string 
     s 
     (lambda ()
       (let loop ((ch (read-char)))
         (cond
          ((eof-object? ch)
           #f)
          ((char-pred ch)
           #t)
          (else
           (loop (read-char)))))
       )))
  (define (has-vowel? s)
    (string-contains-matching-char? 
     (lambda (c)
       (case (char-downcase c)
         ((#\a #\e #\i #\o #\u) #t)
         (else #f))) s ))
  (define (has-non-letter? s)
    (string-contains-matching-char? 
     (lambda (c)
       (or (char-ci<? c #\a)
           (char-ci>? c #\z)))
     s))
  
  
  (let ((l (string-length word)))
    (and (not (zero? l))
             
         ;; it's gotta have a vowel.
         (has-vowel? word)
             
         ;; it's gotta be all letter, all the time.
         (not (has-non-letter? word))
             
         ;; it's gotta be two letters long, unless it's `i' or `a'.
         (or (string-ci=? "i" word)
             (string-ci=? "a" word)
             (< 1 l)))))

(define (bag-acceptable? this bag-to-meet)
  (and (or (bags=? bag-to-meet this)
           (subtract-bags bag-to-meet this))
       this))

(define (init bag-to-meet dict-file-name)
  (if (not *big-ol-hash-table*)
      (set! *big-ol-hash-table* (wordlist->hash dict-file-name)))
  
  (fprintf status-port "Pruning dictionary ... ") (flush-output)

  (set! *dictionary* 
        (let ((entries-examined 0))
          (let ((result (srfi-1-filter (lambda (entry)
                                         (if (zero? (remainder entries-examined 1000))
                                             (fprintf status-port
                                                      (format "Pruning dictionary ... ~a words ..." entries-examined)))
                                         (set! entries-examined (+ 1 entries-examined))
                                         (bag-acceptable? (car entry) bag-to-meet))
                                       (hash-table-map *big-ol-hash-table* cons))))
            result)))
  (fprintf status-port "Pruning dictionary ... done.")
  
  (let ()
    (define (biggest-first e1 e2) 
      (let* ((s1 (cadr e1))
             (s2 (cadr e2))
             (l1 (string-length s1))
             (l2 (string-length s2)))
        (or (> l1 l2)
            (and (= l1 l2)
                 (string<? s1 s2)))))
    (define (shuffled seq)
      (let ((with-random-numbers (map (lambda (item)
                                        (cons (random 2147483647)
                                              item))
                                      seq)
                                 ))
        (set! with-random-numbers (list-quicksort with-random-numbers (lambda (a b)
                                                                        (< (car a)
                                                                           (car b)))))
        (map cdr with-random-numbers)))
    
    (set! *dictionary* 
          (if #t
              (list-quicksort *dictionary* biggest-first)
            (shuffled *dictionary*))
          )))


