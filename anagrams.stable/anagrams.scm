;; Ernest Hemingway -> Sting me anywhere

(require 'filter)
(require 'sort)
(require 'pretty-print)

(load "ss.scm")

(define x
  
  (let ()
    (define (surveyize str)
      (cons str (survey str)))
    (define (verbose-read filename)
      (display (string-append "Reading `"
                              filename
                              "'..."))
      (let ((result (with-input-from-file filename read)))
        (display "done")
        (newline)
        result))

    (define what-to-display '(boredom-relief))
    (define (my-display level . stuff) (if (memq level what-to-display) (for-each display stuff)))
    (define (my-newline level    ) (if (memq level what-to-display) (newline)))

    (let ((wordlist ((lambda (proc seq)
                       (my-display 'boredom-relief "Counting letters in all words in dictionary...")
                       (let ((result (map proc seq)))
                         (my-display 'boredom-relief "done")
                         (newline)
                         result))
                     surveyize
                     (verbose-read "dict.scm"))))
      (lambda (str)

        (define (unique seq equal?)
          (let loop ((seq seq)
                     (result '()))
            (cond
             ((null? seq)
              result)
             ((null? (cdr seq))
              (append seq result))
             ((equal? (car seq)
                      (cadr seq))
              (begin
                (my-display 'boredom-relief "Removing redundant element "  (car seq))
                (my-newline 'boredom-relief)
                (loop (cdr seq)
                      result)))
             (#t
              (loop (cdr seq)
                    (cons (car seq)
                          result))))))

        (define (letter-count str)
          (if (= 0 (string-length str))
              0
            (+
             (if (char-alphabetic? (string-ref str 0))
                 1
               0)
             (letter-count (substring str 1 (string-length str))))))

        (define (prune s/v dictionary)

          (define (too-damned-short? word)
            (< (string-length word)
               3))
          
          (define (begins-with-digit? word)
            (char-numeric? (string-ref word 0)))

          (define (count-matching-chars predicate word)
            (if (= 0 (string-length word))
                0
              (+
               (if (predicate (string-ref word 0)) 1 0)
               (count-matching-chars predicate (substring word 1 (string-length word))))))

          (define (number-of-upper-case-letters word)
            (count-matching-chars char-upper-case? word))

          (define (number-of-vowels word)
            (define (char-vowel? ch)
              (case (char-downcase ch)
                ((#\a #\e #\i #\o #\u #\y) #t)
                (else
                 #f)))
            (count-matching-chars char-vowel? word))

          (define (no-vowels? word)
            (= 0 (number-of-vowels word)))

          (define (all-upper-case? word)
            (= (number-of-upper-case-letters word)
               (string-length word)))

          (define (any-upper-case? word)
            (> (number-of-upper-case-letters word)
               0))

          (define (state-abbreviation? word)
            (and
             (= 2 (string-length word))
             (char-upper-case? (string-ref word 0))
             (char-upper-case? (string-ref word 1))))

          (define (letter-name-plural? word)
            (and
             (= 3 (string-length word))
             (char-alphabetic? (string-ref word 0))
             (char=? #\'       (string-ref word 1))
             (char-ci=? #\s    (string-ref word 2))))

          (define (unacceptable-single-letter? word)
            (and
             (= 1 (string-length word))
             (not (or
                   (string-ci=? word "I")
                   (string-ci=? word "a")))))

          (display "Pruning...")
          (newline)

          (filter (lambda (string/vec)
                    (let ((word (car string/vec)))
                      (and
                       (not (too-damned-short? word))
                       (not (begins-with-digit? word))
                       (not (any-upper-case? word))
                       (not (letter-name-plural? word))
                       (not (unacceptable-single-letter? word))
                       (not (no-vowels? word))
                       (is-contained-in? string/vec s/v)

                       (my-display 'boredom-relief "kept word `"
                                    word
                                     "'")
                       (my-newline 'boredom-relief)
                       word)))
                  dictionary))
        
        (define (make-readable anagram-list)

          (define glue-strings
            (lambda (seq)
              (cond ((null? seq) "")
                    ((null? (cdr seq)) (car seq))
                    (#t (string-append (car seq) " " (glue-strings (cdr seq)))))))

          (map
           (lambda (anagram)
             (glue-strings
              (map car anagram)))
           anagram-list))

        (define (anagrams string/vec dictionary level)
          
          (define (find-first-word s/v dictionary)
            (cond
             ((null? dictionary)
              '())
             ((is-contained-in? (car dictionary)
                                s/v)
              dictionary)
             (#t
              (find-first-word s/v (cdr dictionary)))))

          (define bleep
            (if #f (lambda () #f)
              (let ((counter 0))
                (lambda ()
                  (display (string-ref "\\|/-" (remainder counter 4)))
                  (display "\r")
                  (set! counter (+ 1 counter))))))
          
          (cond
           ((= 0 (string-length (car string/vec)))
            '())
           ((not (char-alphabetic? (string-ref (car string/vec) 0)))
            (anagrams (substring (car string/vec) 1 (string-length (car string/vec)))
                      dictionary
                      level))
           (#t
            (let loop ((dictionary dictionary)
                       (result '())
                       (iterations 0))

              (bleep)
              
              (let ((subdict (find-first-word string/vec dictionary)))
                (if (null? subdict)
                    result
                  (let* ((smaller-pile-of-letters (subtract-letters string/vec (car subdict)))
                         (from-smaller-pile
                          (if (= 0 (string-length (car smaller-pile-of-letters)))
                              (list (list (car subdict)))
                            (let ((more-anagrams (map (lambda (anagram)
                                                        (cons (car subdict) anagram))
                                                      (anagrams smaller-pile-of-letters subdict (+ 1 level)))))
                              (if (and
                                   (not (null? more-anagrams))
                                   (= 0 level))
                                  (begin
                                    (write (make-readable more-anagrams))
                                    (newline)))
                              more-anagrams))))

                    (loop (cdr subdict)
                          (append result from-smaller-pile)
                          (+ 1 iterations)))))))))

        (let* ((s/v (surveyize str))
               (pruned-dictionary
                (unique
                 (sort (prune s/v wordlist)
                       (lambda (w1 w2)
                         (< (letter-count (car w1))
                            (letter-count (car w2)))))
                 (lambda (e1 e2)
                   (string-ci=? (car e1)
                                (car e2))))))

          (call-with-output-file str
            (lambda (port)
              (write (make-readable (anagrams s/v
                                              pruned-dictionary
                                              0))
                     port))))))))

