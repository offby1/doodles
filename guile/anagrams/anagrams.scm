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
        (for-each display (list "done" #\newline)) (force-output)
        result))

    (define what-to-display '(boredom-relief))
    (define (my-display level . stuff) 
      (if (memq level what-to-display) 
          (begin 
            (for-each display stuff) 
            (newline)
            (force-output))))

    (let ((wordlist (verbose-read "dict.scm")))
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
                (loop (cdr seq)
                      result)))
             (#t
              (loop (cdr seq)
                    (cons (car seq)
                          result))))))

        (define (moby-letter-count str)
          (let loop ((chars-examined 0)
                     (total 0))
            (if (= chars-examined (string-length str))
                total
              (loop (+ 1 chars-examined)
                    (+ (if (char-alphabetic? (string-ref str chars-examined)) 1 0)
                       total)))))

        (define bleep
          (if #f
              (let ((counter 0))
                (lambda ()
                  (for-each display (list (string-ref "\\|/-" (remainder counter 4)) "\r"))
                  (force-output)
                  (set! counter (+ 1 counter))))
            (lambda () #f)))

        (define (prune word dictionary)

          (define (begins-with-digit? word)
            (char-numeric? (string-ref word 0)))

          (define (moby-count-matching-chars predicate word)
            (let loop ((chars-examined 0)
                       (total 0))
              (if (= chars-examined (string-length word))
                  total
                (loop (+ 1 chars-examined)
                      (+ (if (predicate (string-ref word chars-examined)) 1 0)
                         total)))))

          (define (number-of-upper-case-letters word)
            (moby-count-matching-chars char-upper-case? word))

          (define (number-of-vowels word)
            (define (char-vowel? ch)
              (case (char-downcase ch)
                ((#\a #\e #\i #\o #\u #\y) #t)
                (else
                 #f)))
            (moby-count-matching-chars char-vowel? word))

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

          (filter (lambda (candidate)
                    (bleep)
                    (and
                     ;; (not (begins-with-digit? candidate))
                     ;; (not (any-upper-case? candidate))
                     ;; (not (letter-name-plural? candidate))
                     ;; (not (unacceptable-single-letter? candidate))
                     ;; (not (no-vowels? candidate))
                     (is-contained-in? candidate word)

                     (my-display 'boredom-relief "kept word `"
                                 candidate
                                 "'")
                     candidate))
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
               anagram))
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

          (cond
           ;; The empty string has no anagrams.
           ((= 0 (string-length string/vec))
            '())
           
           ;; If the first character isn't a letter, skip it and start
           ;; over.
           ((not (char-alphabetic? (string-ref string/vec 0)))
            (anagrams (substring string/vec 1 (string-length string/vec))
                      dictionary
                      level))
           
           (#t
            (let loop ((dictionary dictionary)
                       (result '()))
              
              (bleep)
              
              (let ((subdict (find-first-word string/vec dictionary)))
                (my-display 'debugging "subdict: " subdict)
                (if (null? subdict)
                    (begin
                      (my-display 'debugging
                       "Subdict is empty; returning `"
                       result
                       "'"
                       #\newline)
                      result)
                  (let* ((smaller-pile-of-letters 
                          (let ((x (subtract-letters string/vec (car subdict))))
                            (my-display
                             'debugging
                             "smaller-pile-of-letters: "
                             x
                             #\newline)
                            x))

                         (from-smaller-pile
                          
                          (let ((x
                                 (if (= 0 (string-length smaller-pile-of-letters))
                                     (list (list (car subdict)))
                                   (let ((more-anagrams (map (lambda (anagram)
                                                               (cons (car subdict) anagram))
                                                             (anagrams smaller-pile-of-letters subdict (+ 1 level)))))
                                     (if (and
                                          #f
                                          (not (null? more-anagrams))
                                          (= 0 level))
                                         (begin
                                           (write (make-readable more-anagrams))
                                           (newline)))

                                     more-anagrams))))
                            (my-display
                             'debugging
                             "from-smaller-pile: "
                             x)
                            x)))

                    (my-display 'debugging 
                                "Found subword `"
                                (car subdict)
                                "' of `"
                                string/vec
                                "'")
                    (loop (cdr subdict)
                          (append result from-smaller-pile)))))))))

        (let* ((pruned-dictionary
                (unique
                 
                 ;; sort the dictionary so that longer entries come
                 ;; first.  This makes anagrams with long words appear
                 ;; first, and that's good because they tend to be
                 ;; more interesting.
                 (sort (prune str wordlist)
                       (lambda (w1 w2)
                         (< (moby-letter-count w1)
                            (moby-letter-count w2))))
                 (lambda (e1 e2)
                   (string-ci=? e1
                                e2)))))

          (call-with-output-file 
              str
            (lambda (p)
              (write (make-readable (anagrams str
                                              pruned-dictionary
                                              0))
                     p
                     ))))))))
