(require 'common-list-functions)        ; for reduce
(require 'sort)
(require 'random)

(define falsehoods 
  (make-vector 1000003 #f)
  ;;'()
  )

;; Try either
;;
;;    (start-from "This computer-generated pangram contains")
;;
;; or
;;
;;    (start-from (list "This sentence contains exactly " #\a ", " #\b ", " #\c ", " #\t " and " #\space "."))
;;
;; The first will probably not yield any results; the second may or may not.
(define (start-from seed . options)

  (define save-surveys (not (memq 'save-strings options)))
  (define alist-surveys (not (memq 'vector-surveys options)))

  (define survey:add! #f)

  ;; A survey is a data abstraction -- it might be a procedure.  If it
  ;; is, it wouldn't do to simply copy the procedure to our collection
  ;; of surveys; instead we need to explicitly ask the survey to
  ;; generate a Lisp object that we can save.  That's what the
  ;; `expose' method does.  The requirement is that the "exposed"
  ;; objects from two surveys made from the same sentence should be
  ;; `equal?'.

  (define survey:expose #f)
  (define survey:make #f)
  (define survey:ref #f)
  (define survey:size #f)

  (if alist-surveys
      (begin
        (define (make-alist-survey)

          (let ((value '()))
            (define (survey:add! s c)
              (if (null? s)
                  (list (cons c 1))
                (let ((old-value (assq c s)))
                  (if old-value
                      (begin
                        (set-cdr! old-value (+ 1 (cdr old-value)))
                        s)
                    (cons (cons c 1)
                          s)))))

            ;;(trace survey:add!)
            (lambda (op . args)
              (case op
                ((non-zero-elements)
                 (length value))

                ((add)
                 (set! value (survey:add! value (car args))))

                ((ref)
                 (let ((cell (assq (car args) value)))
                   (if cell
                       (cdr cell)
                     0)))

                ((expose)

                 ;; return a value suitable for comparison with
                 ;; `equal?'

                 (sort value
                       (lambda (foo bar)
                         (char<? (car foo)
                                 (car bar)))))

                (else
                 (error "Unknown op for survey: " op))))))

        (set! survey:make (lambda ()
                            (let ((return (make-alist-survey)))
                                        ;(trace return)
                              return)))

        (set! survey:expose (lambda (s)
                              (s 'expose)))

        (set! survey:size (lambda (s)
                            (s 'size)))

        (set! survey:add! (lambda ( s c)
                            (s 'add c)))

        (set! survey:ref (lambda ( s c)
                           (s 'ref c))))

    (begin
      (set! survey:make (lambda ()
                          (make-vector char-code-limit 0)))

      (set! survey:expose (lambda (s)
                            s))

      (set! survey:size vector-length )

      (set! survey:add! (lambda ( s c)
                          (let* ((index (char->integer c)))
                            (vector-set! s index (+ 1 (vector-ref s index))))))

      (set! survey:ref (lambda( s c)
                         (vector-ref s (char->integer c))))))

  (define (make-doublet thing1 thing2)

    (let ((value (cons thing1 thing2)))
      (lambda ()
        ((if save-surveys car cdr) value))))

  (define (add-to-falsehoods! datum)
    (hash-set! falsehoods datum #t)
    ;;(set! falsehoods (cons datum falsehoods))
                                             )

  (define (known-false? datum)
    (hash-ref falsehoods datum)
    ;;(member datum collection)
                             )

  ;;(trace known-false?)

  (define (time-pair->seconds p)
    (+ (car p)
       (/ (cdr p)
          1000000)))

  (let ((start-time (time-pair->seconds (gettimeofday))))

    (define (report-speed f)
      (let ((elapsed-time (- (time-pair->seconds (gettimeofday))
                             start-time)))
        (define (safe-quotient n d)
          (if (zero? d)
              "a lot"
            (/ n d)))
        (display (safe-quotient f elapsed-time))
        (display "\r")
        (force-output)))

    (define (string->survey s)
      (let ((length (string-length s))
            (the-survey (survey:make)))

        ;; if our survey doesn't contain a cell for this character
        ;;   add a cell with value "1".
        ;; else
        ;;   increment the appropriate cell

        (let loop ((chars-processed 0))
          (if (< chars-processed length)
              (begin
                (survey:add! the-survey (char-downcase (string-ref s (- length chars-processed 1))))
                (loop (+ 1 chars-processed)))))
        the-survey))

       ;;(trace string->survey)

    (define (number-spelling n)
      (let* ((some-spellings '((0 . "zero")
                               (1 . "one")
                               (2 . "two")
                               (3 . "three")
                               (4 . "four")
                               (5 . "five")
                               (6 . "six")
                               (7 . "seven")
                               (8 . "eight")
                               (9 . "nine")
                               (10 . "ten")
                               (11 . "eleven")
                               (12 . "twelve")
                               (13 . "thirteen")
                               (14 . "fourteen")
                               (15 . "fifteen")
                               (16 . "sixteen")
                               (17 . "seventeen")
                               (18 . "eighteen")
                               (19 . "nineteen")
                               (20 . "twenty")
                               (30 . "thirty")
                               (40 . "forty")
                               (50 . "fifty")
                               (60 . "sixty")
                               (70 . "seventy")
                               (80 . "eighty")
                               (90 . "ninety")))
             (cell (assq n some-spellings)))

        (define (filter predicate sequence)
          (cond ((null? sequence) '())
                ((predicate (car sequence))
                 (cons (car sequence)
                       (filter predicate (cdr sequence))))
                (else (filter predicate (cdr sequence)))))

        ;; If our number is in the list, great; we're done.
        (if cell (cdr cell)
          (cond  ((< n 100)
                  ;; Find the largest spelling that's less than n, and display
                  ;; that, followed by a hyphen; then call ourselves
                  ;; recursively on the remainder.
                  (let* ((best-fit-cell (let ((last (lambda (list)
                                                      (list-ref list
                                                                (- (length list)
                                                                   1)))))
                                          (last (filter (lambda (x)
                                                          (< (car x) n))
                                                        some-spellings))))
                         (smaller (- n (car best-fit-cell))))
                    (string-append (cdr best-fit-cell)
                                   "-"
                                   (number-spelling smaller))))
                 ((< n 1000)
                  ;; Figure out how many hundreds; display the spelling of
                  ;; that number, followed by the spelling of 100.
                  ;; Subtract those hundreds and call ourselves
                  ;; recursively -- unless the difference is zero.
                  (let* ((hundreds (quotient n 100))
                         (smaller (- n (* 100 hundreds))))
                    (apply string-append
                           (cons (number-spelling hundreds)
                                 (cons " hundred"
                                       (if (not (= 0 smaller))
                                           (cons " " (list
                                                      (number-spelling smaller)))
                                         '()))))))
                 ((< n 10000)
                  ;; similar to the above with hundreds.
                  (let* ((thousands (quotient n 1000))
                         (smaller (- n (* 1000 thousands))))
                    (apply string-append
                           (cons (number-spelling thousands)
                                 (cons " thousand"
                                       (if (not (= 0 smaller))
                                           (cons " " (list (number-spelling smaller)))
                                         '()))))))
                 (#t "dunno")))))

    (define (character-spelling c number)
      (define (pluralize thing)
        (cond ((string? thing)
               (string-append thing (if (= number 1)
                                        ""
                                      (string-append
                                       ;; Return an apostrophe if our thing is a single alphabetic
                                       ;; character.
                                       (if (and
                                            (= 1 (string-length thing))
                                            (char-alphabetic? (string-ref thing 0)))
                                           "'"
                                         "")
                                       "s"))))
              ((pair? thing)
               (if (= number 1)
                   (car thing)
                 (cdr thing)))
              (else
               (error
                "Pluralize doesn't know how to deal with "
                thing))))
      (pluralize
       (cond
        ((or (char-alphabetic? c)
             (char-numeric? c))
         (string c))
        (else
         (case c
           ((#\space)
            "space")
           ((#\newline)
            "newline")
           ((#\tab)
            "tab")
           ((#\!)
            "exclamation point")
           ((#\")
            "double-quote")
           ((#\#)
            "pound sign")
           ((#\$)
            "dollar sign")
           ((#\%)
            "percent sign")
           ((#\&)
            "ampersand")
           ((#\')
            ;; "single-quote"
            "apostrophe"
            )
           ((#\()
            '("left parenthesis" . "left parentheses"))
           ((#\))
            '("right parenthesis" .  "right parentheses"))
           ((#\*)
            "asterisk")
           ((#\+)
            "plus sign")
           ((#\,)
            "comma")
           ((#\-)
            "hyphen")
           ((#\.)
            "period")
           ((#\/)
            '("slash" . "slashes"))
           ((#\:)
            "colon")
           ((#\;)
            "semi-colon")
           ((#\<)
            "less-than sign")
           ((#\=)
            "equals sign")
           ((#\>)
            "greater-than sign")
           ((#\?)
            "question mark")
           ((#\@)
            "at sign")
           ((#\[)
            "left bracket")
           ((#\\)
            '("backslash" . "backslashes"))
           ((#\])
            "right bracket")
           ((#\^)
            "caret")
           ((#\_)
            "underscore")
           ((#\`)
            "acute accent")
           ((#\{)
            "left curly-brace")
           ((#\|)
            "vertical bar")
           ((#\})
            "right curly-brace")
           ((#\~)
            "tilde")
           (else
            "goofy character"))))))

    (define (template->initial-string t)
      (if (string? t)
          t
        (let loop ((t t)
                   (result ""))
          (if (null? t)
              result
            (loop (cdr t)
                  (let* ((initial-value (lambda ()
                                          (random 40)))
                         (init (initial-value)))
                    (string-append result (if (string? (car t))
                                              (car t)
                                            (string-append (number-spelling init)
                                                           " "
                                                           (character-spelling (car t)
                                                                               init))))))))))

    (let loop ((s (template->initial-string seed))
               (surv (string->survey (template->initial-string seed)))
               (falsehoods-discovered 0))

      (let ((doublet (make-doublet (survey:expose surv) s)))
        (define (survey->string srv template)
          (if (string? template)

              ;; If the template is a string, simply count up all the non-zero
              ;; characters in the survey, spell those counts out, and append
              ;; those spellings to the template.

              (let loop ((chars-processed 0)

                         ;; a list of strings that look like, for example, "four a's".
                         (char-count-spellings '()))

                ;; Given '(), return "".
                ;; given ("one thing"), return "one thing"
                ;; given ("one thing" "another thing") return "one thing and another thing".
                ;; given ("1" "2" "3") return "1, 2, and 3".
                ;; given ("1" "2" ... "n" ) return "1, 2, ..., n-1, and n".

                ;; Or in English: this function combines its arguments somewhat the
                ;; way (lambda (string-list) (reduce string-append string-list))
                ;; would, but it adds commas, spaces, and the word "and" appropriately
                ;; for English.

                (define (enumerate string-list)
                  (case (length string-list)
                    ((0)
                     "")
                    ((1)
                     (car string-list))

                    ;; This clause is optional.  If you omit it, you get a
                    ;; comma after the first element of a two-element list.
                    ;; If you include it, you don't get that comma.
                    ((2) (string-append (car string-list) " and " (cadr string-list)))

                    (else
                     (string-append (reduce string-append (map (lambda (s)
                                                                 (string-append s ", "))
                                                               (butlast string-list 1)))
                                    "and "
                                    (car (last string-list 1))))))
                (if (= chars-processed char-code-limit)
                    (string-append template
                                   (enumerate char-count-spellings)
                                   ".")

                  (let ((this-char-count (survey:ref srv (integer->char chars-processed))))

                    (loop (+ 1 chars-processed)
                          (append char-count-spellings
                                  (if (> this-char-count 0)

                                      (list (string-append
                                             (number-spelling this-char-count)
                                             " "
                                             (character-spelling (integer->char chars-processed) this-char-count)))
                                    '()))))))

            ;; The template is a list of strings and characters.  Make a
            ;; string that contains the strings from the template, and the
            ;; spellings of the counts of the characters in the template.

            (let loop ((template template)
                       (return ""))
              (if (null? template)
                  return

                ;; if it's a string, append it to our return value
                (loop (cdr template)
                      (string-append return (if (string? (car template))
                                                (car template)
                                              (string-append (number-spelling (survey:ref srv (car template)))
                                                             " "
                                                             (character-spelling (car template)(survey:ref srv (car template)) )))))))))
           ;;(trace survey->string)
        (if (known-false? (doublet))
            (begin
              (report-speed falsehoods-discovered)
              falsehoods-discovered)
          (let ((next (survey->string surv seed)))
            (if (string=? s next)
                (begin
                  (display s)
                  (newline)
                  s)
              (begin
                (add-to-falsehoods! (doublet))
                (loop next
                      (string->survey next)
                      (+ 1 falsehoods-discovered))))))))))

(define (grind seed)
  (let ((result (start-from seed)))
    (if (not (string? result))
        (grind seed)
      (begin
        ;; trumpet flourish
        (display "Hey, I found one!\n")
        (display result)
        (newline)
        result))))

(define seed
  (list "This sentence honoring Katie Drake contains exactly "
        ;; #\  ", "
        ;; #\! ", "
        ;; #\" ", "
        ;; #\# ", "
        ;; #\$ ", "
        ;; #\% ", "
        ;; #\& ", "
        ;; #\( ", "
        ;; #\) ", "
        ;; #\* ", "
        ;; #\+ ", "
        ;; #\/ ", "
        ;; #\0 ", "
        ;; #\1 ", "
        ;; #\2 ", "
        ;; #\3 ", "
        ;; #\4 ", "
        ;; #\5 ", "
        ;; #\6 ", "
        ;; #\7 ", "
        ;; #\8 ", "
        ;; #\9 ", "
        ;; #\: ", "
        ;; #\; ", "
        ;; #\< ", "
        ;; #\= ", "
        ;; #\> ", "
        ;; #\? ", "
        ;; #\@ ", "
        ;; #\[ ", "
        ;; #\\ ", "
        ;; #\] ", "
        ;; #\^ ", "
        ;; #\_ ", "
        ;; #\` ", "
        #\a ", "
        ;; #\b ", "
        ;; #\c ", "
        ;; #\d ", "
        #\e ", "
        ;; #\f ", "
        ;; #\g ", "
        ;; #\h ", "
        ;; #\i ", "
        ;; #\j ", "
        ;; #\k ", "
        ;; #\l ", "
        ;; #\m ", "
        ;; #\n ", "
        ;; #\o ", "
        ;; #\p ", "
        ;; #\q ", "
        ;; #\r ", "
        ;; #\s ", "
         #\t ", "
        ;; #\u ", "
        ;; #\v ", "
        ;; #\w ", "
        ;; #\x ", "
        ;; #\y ", "
        ;; #\z ", "
        ;; #\- ", "
        ;; #\' ", "
        ;; #\, ", "
        #\. ", and "
        #\ "."
        ))