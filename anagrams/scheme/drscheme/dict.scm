(module dict
    mzscheme
  (require "bag.scm"
           (prefix list- (lib "list.ss"))
           (lib "pretty.ss")
           (lib "process.ss")
           (lib "mred.ss" "mred")
           (prefix srfi-1- (lib "1.ss" "srfi"))
           (prefix srfi-13- (lib "13.ss" "srfi")))
  (provide dict-for-each init)
  
  (define *big-ol-hash-table* #f)

  (define (wordlist->hash fn)
    (with-input-from-file fn
      (lambda ()
        (let ((dict (make-hash-table 'equal)))
          (fprintf (current-error-port) (format "Reading dictionary ... "))
          (let loop ((word  (read-line))
                     (words-read 0))
            (if (eof-object? word)
                (fprintf (current-error-port) (format "Reading dictionary ... done."))
              (begin
                (if (word-acceptable? word)
                    (adjoin-word dict (srfi-13-string-downcase word)))
                (if (zero? (remainder words-read 1000))
                    (fprintf (current-error-port)  (format "Reading dictionary ... ~a words ..." words-read)))
                (loop (read-line)
                      (+ 1 words-read)))
              ))
          dict)
        )))
  
  (define *alist* #f)
  
  (define (dict-for-each proc)
    "An iterator over dictionary elements.  Applies PROC
successively on all dictionary items.  The arguments to PROC are
\"(key value)\" where key and value are successive pairs from the
dictionary."
    (for-each (lambda (p)
                (proc (car p)
                      (cdr p))) *alist*))
  
  (define (adjoin-word dict word)
    (let* ((this-bag (bag word))
           (probe (hash-table-get dict this-bag (lambda () #f))))
      (cond
       ((not probe)
        (hash-table-put! dict this-bag (list-quicksort (list word) string<?)))
       ((not (member word probe))
        (hash-table-put! dict this-bag (cons word probe)))
       )))
  
  (define word-acceptable?
    (let ((has-vowel-regexp (regexp "[aeiouAEIOU]"))
          (has-non-ASCII-regexp (regexp "[^a-zA-Z]"   )))
      (lambda (word)
        (let ((l (string-length word)))
          (and (not (zero? l))
               
               ;; it's gotta have a vowel.
               (regexp-match has-vowel-regexp word)
               
               ;; it's gotta be all ASCII, all the time.
               (not (regexp-match has-non-ASCII-regexp word))
               
               ;; it's gotta be two letters long, unless it's `i' or `a'.
               (or (string=? "i" word)
                   (string=? "a" word)
                   (< 1 l)))))))
  
  
  (define (bag-acceptable? this bag-to-meet)
    (and (or (bags=? bag-to-meet this)
             (subtract-bags bag-to-meet this))
         this))
  
  (define (init bag-to-meet dict-file-name)
    (if (not *big-ol-hash-table*)
        (set! *big-ol-hash-table* (wordlist->hash dict-file-name)))
    
    (fprintf (current-error-port) "Pruning dictionary ... ") (flush-output)

    (set! *alist* 
          (let ((entries-examined 0))
            (let ((result (srfi-1-filter (lambda (entry)
                                           (if (zero? (remainder entries-examined 1000))
                                               (fprintf (current-error-port)
                                                         (format "Pruning dictionary ... ~a words ..." entries-examined)))
                                           (set! entries-examined (+ 1 entries-examined))
                                           (bag-acceptable? (car entry) bag-to-meet))
                                         (hash-table-map *big-ol-hash-table* cons))))
              result)))
    (fprintf (current-error-port) "Pruning dictionary ... done.")
    
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
      
      (set! *alist* 
            (if #t
                (list-quicksort *alist* biggest-first)
              (shuffled *alist*))
            )))

  
  )
