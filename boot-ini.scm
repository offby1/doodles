(require 'parse-ini)
(require 'whitespace)
(require 'sort)


;; assuming boot.ini has exactly one pair of OS entries which differ
;; only in that one has "/safemode" and the other doesn't, *and* if
;; one of those entries is the current default, replaces the current
;; default with the other entry.

(define (maybe-toggle boot.ini)
  
  (define (assoc-ci str seq)
    (cond
     ((null? seq)
      #f)
     ((string-ci=? str (car (car seq)))
      (car seq))
     (#t
      (assoc-ci str (cdr seq)))))

  ;; returns a list of pairs.  Each pair is a string and a number.  No
  ;; string appears more than once in the returned list.  Each string
  ;; appears at least once in the input STRINGS, an the number is how
  ;; many times it appeared in STRINGS.

  ;; So if the input is ("bob" "harvey" "bob"), the output will be
  ;; (("bob" . 2) ("harvey" . 1))
  (define (survey strings)
    (let ((result '()))
      (let loop ((strings strings))
      
        (if (null? strings)
            result
          (begin
            (let ((entry (assoc-ci (car strings) result)))
              (if entry
                  (set-cdr! entry (+ 1 (cdr entry)))
                (set! result (cons (cons (car strings)
                                         1)
                                   result))))
            (loop (cdr strings)))))))
  (define (is-boot.ini? x)

    (define (boot-loader-section boot.ini)
      (assoc-ci "boot loader" x))

    (define (operating-systems-section boot.ini)
      (assoc-ci "operating systems" x))

    (let ((bls (boot-loader-section x))
          (os-section (operating-systems-section x)))
      (and (= 2 (length x))
           bls
           os-section
           (assoc-ci "timeout" (cdr bls))
           (assoc-ci "default" (cdr bls))
           #t)))

  (if (not (is-boot.ini? boot.ini))
      (error "That ain't no boot.ini file:" boot.ini))

  (let* ((os-entries  (cdadr boot.ini))
         (counts (survey (map car os-entries)))
         (entries-that-appear-exactly-twice (filter (lambda (p) (= (cdr p) 2))
                                             counts)))
    
    (if (not (= 1 (length entries-that-appear-exactly-twice)))
        #f
      ;; now find the strings associated with this entry
      (let ((the-two-entries
             (filter (lambda (os-entry)
                       (string-ci=? (car os-entry) (caar entries-that-appear-exactly-twice)))
                     os-entries)))

        (if (not (= 2 (length the-two-entries)))
            (error "This program is confused."))
        
           ;; Now see if one has "safemode", and the other doesn't.
        (let ()
          (define (has-safemode-switch str)
            ;; returns a list of substrings of str.
            ;; each appears in str beginning with a slash, and terminated by
            ;; another slash, whitespace, or the end of the string.

            (define (switches str)

              ;; Kind of like what AWK does -- split the input string into a list of
              ;; strings.

              ;; (split "My dog has fleas" char-whitespace?) -> ("My" "dog" "has" "fleas")
              ;; (split "" char-whitespace?) -> ()
              ;; (split "     " char-whitespace?) -> ()
              ;; (split "3.14" (lambda (c) (not (char-numeric? c)))) -> ("3" "14")

              ;; (map (lambda (l)
              ;;        (split l char-whitespace?))
              ;;      (split "Once upon a time, Fred had kittens" char-upper-case? #t))
              ;;
              ;; ->
              ;; (("Once" "upon" "a" "time,") ("Fred" "had" "kittens"))


              (define (split string separator? . options)

                (define (cons-if-not-empty str seq)
                  (if (not (= 0 (string-length str)))
                      (cons str seq)
                    seq))

                (define keep-separators (memq #t options))

                (reverse
                 (let loop ((str string)
                            (result '())
                            (accumulated-word ""))

                   (if (= 0 (string-length str))
                       (cons-if-not-empty accumulated-word result)
                     (let ((char (string-ref str 0)))
                       (loop
                        (substring str 1 (string-length str))

                        (if (separator? char)
                            (cons-if-not-empty accumulated-word result)
                          result)

                        ;; otherwise glue the character onto the accumulated word
                        (if (separator? char)
                            (if keep-separators
                                (make-string 1 char)
                              "")
                          (string-append accumulated-word (make-string 1 char)))))))))
              (map strip-whitespace
                   (filter (lambda (s) (char=? #\/ (string-ref s 0)))
                           (split str
                                  (lambda (c)
                                    (or
                                     (char-whitespace? c)
                                     (char=? c #\/)))
                                  #t))))            
            (define (ci-member str lst)
              (cond
               ((null? lst)
                #f)
               ((string-ci=? str (car lst))
                lst)
               (#t
                (ci-member str (cdr lst)))))
            (ci-member "/safemode" (switches str)))
          (define (verbose-xor a b)
            (cond
             ((and a b)
              #f)
             ((and (not a)
                   (not b))
              #f)
             (#t #t)
             )
            )
          
          (if
              (and
               (verbose-xor (has-safemode-switch (cdar  the-two-entries))
                            (has-safemode-switch (cdadr the-two-entries)))
               (string-ci=? (cdr (assoc-ci "default" (cdr (assoc-ci "boot loader" boot.ini))))
                            (caar the-two-entries)))
              ;; OK, let's interchange the two values.
              (let* ((one-entry (car the-two-entries))
                     (another-entry (cadr the-two-entries))
                     (temp (cdr one-entry)))

                (set-cdr! one-entry (cdr another-entry))
                (set-cdr! another-entry temp)))
          boot.ini)))))
