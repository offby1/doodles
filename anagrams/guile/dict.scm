(define-module (dict))

(use-modules (ice-9 rdelim)
             (ice-9 pretty-print)
             (srfi srfi-1)
             (bag))

(define *big-ol-hash-table* #f)

(define *alist-file-name* "big-dict-alist.scm")

(define (wordlist->hash)
  (with-input-from-file
      "/usr/share/dict/words"
    (lambda ()
      (let ((dict (make-hash-table 10000)))
        (format #t "Reading dictionary ... ") (force-output)
        (let loop ((word  (read-delimited "\n")))
          (if (eof-object? word)
              (format #t "done~%")
            (begin
              (if (word-acceptable? word)
                  (adjoin-word dict (string-downcase word)))
              (loop (read-delimited "\n")))
            ))
        dict)
      )))

(define-public (dict-for-each proc)
  "An iterator over dictionary elements.  Applies PROC
successively on all dictionary items.  The arguments to PROC are
\"(key value)\" where key and value are successive pairs from the
dictionary."
  (if #f
      (hash-fold (lambda (key words prior)
                   (proc key words))
                 #f
                 *big-ol-hash-table*)
    (for-each (lambda (p)
                (proc (car p)
                      (cdr p))) *alist*)))

(define (adjoin-word dict word)
  (let* ((this-bag (bag word))
         (probe (hash-ref dict this-bag)))
    (cond
     ((not probe)
      (hash-set! dict this-bag (list word)))
     ((not (member word probe))
      (hash-set! dict this-bag (cons word probe)))
     )))

(define word-acceptable?
  (let ((has-vowel-regexp     (make-regexp "[aeiou]" regexp/icase))
        (has-non-ASCII-regexp (make-regexp "[^a-z]"  regexp/icase )))
    (lambda (word)
      (let ((l (string-length word)))
        (and (not (zero? l))

             ;; it's gotta have a vowel.
             (regexp-exec has-vowel-regexp word)

             ;; it's gotta be all ASCII, all the time.
             (not (regexp-exec has-non-ASCII-regexp word))
             
             ;; it's gotta be two letters long, unless it's `i' or `a'.
             (or (string=? "i" word)
                 (string=? "a" word)
                 (< 1 l)))))))

(define (bag-acceptable? this bag-to-meet)
  (and (or (bags=? bag-to-meet this)
           (subtract-bags bag-to-meet this))
       this))

(define-public (init bag-to-meet)
  (set! *big-ol-hash-table* (make-hash-table 10000))
  (format #t "Pruning dictionary ... ") (force-output)
  (for-each
   (lambda (pair)
     (let ((bag (car pair))
           (words (cdr pair)))
       (let ((this-bag (bag-acceptable? bag bag-to-meet)))
         (if this-bag
             (hash-set! *big-ol-hash-table* bag words)))))
   *alist*)
  ;; now clobber the alist.
  (set! *alist* (hash-fold (lambda (key value prior)
                             (cons (cons key value) prior)) 
                           '()
                           *big-ol-hash-table*))
  (format #t "done~%")

  
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
                                        (cons (random:normal)
                                              item))
                                      seq)
                                 ))
        (set! with-random-numbers (sort! with-random-numbers (lambda (a b)
                                                               (< (car a)
                                                                  (car b)))))
        (map cdr with-random-numbers)))

    (format #t "Sorting or shuffling the dictionary (~a elements), for the hell of it ... "
            (length *alist*))
    (set! *alist* 
          (if #t
              (sort! *alist* biggest-first)
            (shuffled *alist*))
          )
    (format #t "done.~%"))
  )

(define-public (test-init)
  (set! *big-ol-hash-table* (make-hash-table 26))
  (for-each (lambda (word)
              (hash-set!
               *big-ol-hash-table*
               (bag word)
               (list word)))
            (map (lambda (i)
               (string (integer->char (+ i (char->integer #\a))))) (iota 26))))

(if (not (access? *alist-file-name* R_OK))
    (begin
      (format #t "Caching dictionary ... ")
      ;; keep the computation of `result' out of the call to
      ;; `with-output-to-file', since that computation might spew
      ;; stuff to the current output port.

      ;; don't use `call-with-output-file' since older versions of
      ;; pretty-print don't accept a port parameter.
      (let ((result (hash-fold (lambda (key value prior)
                                 (cons (cons key value) prior)) 
                               '()
                               (wordlist->hash))))
        (with-output-to-file *alist-file-name*
          (lambda ()
            ;; use `pretty-print' rather than `write' so that it's not
            ;; all on on big line.
            (pretty-print result ))))
      
      (format #t "done~%")))

(define *alist* (with-input-from-file *alist-file-name* read))
(set! *random-state* (seed->random-state (get-internal-real-time)))

;; calculate letter frequencies.  This could be used to re-order the
;; prime-number list in bag.scm.
(if #f
    (map car
         (sort
          (let ((tally '()))
            (for-each
             (lambda (str)
               (let loop ((str str))
                 (if (not (zero? (string-length str)))
                     (let* ((this-letter (string-ref str 0))
                            (probe (assq this-letter tally)))
                    
                       (set! tally (assq-set! tally
                                              this-letter
                                              (+ 1 (if (not probe)
                                                       0
                                                     (cdr probe) ))
                                              ))
                       (loop (substring str 1)
                             )))))
             (apply append (map cdr *alist*)))
            tally)
          (lambda (p1 p2)
            (> (cdr p1)
               (cdr p2)))))
  )