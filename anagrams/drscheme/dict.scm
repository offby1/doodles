(module dict
    mzscheme
  (require "bag.scm"
           (prefix list- (lib "list.ss"))
           (lib "pretty.ss")
           (prefix srfi-1- (lib "1.ss" "srfi"))
           (prefix srfi-13- (lib "13.ss" "srfi")))
  (provide dict-for-each init test-init)

  (define *big-ol-hash-table* #f)

  (define *alist-file-name* "big-dict-alist.scm")

  (if (not (file-exists? *alist-file-name*))
      (begin
        (printf (format "Caching dictionary ... "))
        ;; don't use `call-with-input-file',since pretty-print doesn't
        ;; take a port argument in guile 1.6.4
        (with-output-to-file *alist-file-name*
          (lambda ()
            ;; use `pretty-print' rather than `write' so that it's not all
            ;; on on big line.
            (pretty-print (hash-table-map (wordlist->hash) 
                                          (lambda (key value)
                                             (cons key value))) )))
        
        (printf (format "done~%"))))

  (define *alist* #f)

  (define (wordlist->hash)
    (with-input-from-file
        "d:/cygwin/usr/share/dict/words"
      (lambda ()
        (let ((dict (make-hash-table 'equal)))
          (printf (format "Reading dictionary ... ")) (flush-output)
          (let loop ((word  (read-line)))
            (if (eof-object? word)
                (printf (format "done: ~a~%" dict))
              (begin
                (if (word-acceptable? word)
                    (adjoin-word dict (srfi-13-string-downcase word)))
                (loop (read-line )))
              ))
          dict)
        )))

  (define (dict-for-each proc)
    "An iterator over dictionary elements.  Applies PROC
successively on all dictionary items.  The arguments to PROC are
\"(key value)\" where key and value are successive pairs from the
dictionary."
    (if #f
        (hash-table-for-each proc *big-ol-hash-table*)
      (for-each (lambda (p)
                  (proc (car p)
                        (cdr p))) *alist*)))

  (define (adjoin-word dict word)
    (let* ((this-bag (bag word))
           (probe (hash-table-get dict this-bag)))
      (cond
       ((not probe)
        (hash-table-put! dict this-bag (list word)))
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

  (define (init bag-to-meet)
    (set! *big-ol-hash-table* (make-hash-table 'equal))
    (printf (format "Pruning dictionary ... ")) (flush-output)
    (for-each
     (lambda (pair)
       (let ((bag (car pair))
             (words (cdr pair)))
         (let ((this-bag (bag-acceptable? bag bag-to-meet)))
           (if this-bag
               (hash-table-put! *big-ol-hash-table* bag words)))))
     (with-input-from-file *alist-file-name* read))
    ;; now clobber the alist.
    (set! *alist* (hash-table-map *big-ol-hash-table* 
                             
                                  (lambda (key value )
                                    (cons key value))))
    (printf (format "done~%"))

    
    ;; TODO -- consider sorting *alist*.  There are two ways you might
    ;; want to sort it:

    ;; 1) put the  big words first, so that  the most interesting anagrams
    ;; appear first.

    ;; 2) shuffle randomly, so that when we run the program many times
    ;; with the same input, the anagrams appear in different orders.  This
    ;; is useful for when the input is large, and the complete list of
    ;; anagrams would take forever to generate; in that case, we simply
    ;; let it run for a little while and collect the first few it
    ;; generates.  If it generates different ones each run, we'll not get
    ;; bored.

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

      (printf (format "Sorting or shuffling the dictionary (~a elements), for the hell of it ... "
              (length *alist*)))
      (set! *alist* 
            (if #t
                (list-quicksort *alist* biggest-first)
              (shuffled *alist*))
            )
      (printf (format "done.~%")))
    )

  (define (test-init)
    (set! *big-ol-hash-table* (make-hash-table 'equal))
    (for-each (lambda (word)
                (hash-table-put!
                 *big-ol-hash-table*
                 (bag word)
                 (list word)))
              (map (lambda (i)
                     (string (integer->char (+ i (char->integer #\a))))) (srfi-1-iota 26)))))
