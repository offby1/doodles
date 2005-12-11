(define *big-ol-hash-table* #f)

(define (wordlist->hash fn)

  (define (string-downcase s)
    (with-output-to-string
     ""
     (lambda ()
       (with-input-from-string
        s
        (lambda ()
          (let loop ((ch (read-char)))
            (if (not (eof-object? ch))
                (begin
                  (display (char-downcase ch))
                  (loop (read-char))))))))))

  (with-input-from-file fn
    (lambda ()
      (let ((dict (make-table init: #f
                              test: eq? ; using = as the test works, but is agonizingly slow
                              )))
        (display  "Reading dictionary ... ")
        (let loop ((word  (read-line))
                   (words-read 0))
          (if (eof-object? word)
              (begin
                (display  " done: " (current-error-port))
                (display words-read (current-error-port))
                (display " words."  (current-error-port))
                (newline (current-error-port)))
            (begin
              (if (word-acceptable? word)
                  (adjoin-word dict (string-downcase word)))
              (if (zero? (remainder words-read 1000))
                  (begin
                    (display " ")
                    (display  words-read))
                )
              (loop (read-line)
                    (+ 1 words-read)))
            ))
        dict)
      )))

(define *dictionary* #f)

(define (adjoin-word dict word)
  (let* ((this-bag (bag word))
         (probe (table-ref dict this-bag)))
    (cond
     ((not probe)
      (table-set! dict this-bag (list word)))
     ((not (member word probe))
      (table-set! dict this-bag (cons word probe)))
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

(define (init bag-to-meet dict-file-name)
  (define (filter proc? seq)
    (let loop ((seq seq)
               (result '()))
      (if (null? seq)
          (reverse result)
        (loop (cdr seq)
              (if (proc? (car seq))
                  (cons (car seq)
                        result)
                result)))))
  (if (not *big-ol-hash-table*)
      (set! *big-ol-hash-table* (wordlist->hash dict-file-name)))
  
  (display "Pruning dictionary ... " (current-error-port)) 

  (set! *dictionary* 
        (let ((entries-examined 0))
          (let ((result (filter (lambda (entry)
                                  (if (zero? (remainder entries-examined 1000))
                                      (begin
                                        
                                        (display " ")
                                        (display  entries-examined)
                                        ))
                                  (set! entries-examined (+ 1 entries-examined))
                                  (subtract-bags bag-to-meet (car entry)))
                                (table->list *big-ol-hash-table*))))
            result)))
  (display " done; down to " (current-error-port))
  (display (length *dictionary*) (current-error-port))
  (display " words." (current-error-port))
  (newline (current-error-port)))

