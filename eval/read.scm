;;(define-module (eric read))

;; An approximation to a Scheme lexical analyzer.  The one shortcoming
;; that I know of is that it doesn't recognize `...' as an identifier.

(define letter? char-alphabetic?)
(define (special-initial? c)
  (case c
    ((#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~ #\+ #\-)
     #t)
    (else #f)))

(define (initial? c)
  (or (letter? c)
      (special-initial? c)))

(define digit? char-numeric?)

(define (special-subsequent? c)
  (case c
    ((#\+ #\- #\. #\@)
     #t)
    (else
     #f)))

(define (subsequent? c)
  (or (initial? c)
      (digit? c)
      (special-subsequent? c)))

(define (read-identifier p)
  (string-append (string (read-char p))
                 (port->string p subsequent?)))

;; Can return vector-left-paren, true, false, character, exact,
;; inexact, radix
(define (read-hash-thing p)
  (define (bail-if-eof)
    (if (eof-object? (peek-char p))
        (error "End of file in the middle of a token")))

  (read-char p)                         ; consume the `#'
  
  (bail-if-eof)

  (cond
   ((char=? #\( (peek-char p))
    (read-char p)
    'vector-left-paren)
   ((char-ci=? #\t (peek-char p))
    (read-char p)                       ; consume the `t'
    'true)
   ((char-ci=? #\f (peek-char p))
    (read-char p)                       ; consume the `f'
    'false)
   ((char=? #\\ (peek-char p))
    (read-char p)                       ; consume the `\'
    (bail-if-eof)
    (cons 'character 
          (let ((c (peek-char p)))
            (if (char-alphabetic? c)
                (let ((char-name (port->string p char-alphabetic?)))
                  (cond
                   ((= 1 (string-length char-name))
                    (string-ref char-name 0))
                   ((string=? char-name "newline")
                    #\newline)
                   ((string=? char-name "space")
                    #\ )
                   (else
                    (string-append "Illegal character name: `"
                                   char-name
                                   "'"))))
              (begin
                (read-char p)           ; consume the character
                c)))))
   
   ((char-ci=? #\e (peek-char p))
    (read-char p)
    'exact)
   (else
    'illegal-#-thing)))

(define (read-integer p)
  (string->number (port->string p char-numeric?)))

(define (read-token p)

  (define (eat-whitespace)
    (let loop ((c (peek-char p)))
      (cond
       ((eof-object? c))
       ((or (char=? c #\space)
            (char=? c #\newline)
            (char=? c #\np))
        (read-char p)
        (loop (peek-char p)))
       ((char=? c #\;)
        (read-char p)
        (port->string p
                      (lambda (c)
                        (not (char=? c #\newline))))
        (loop (peek-char p))))))
  
  (eat-whitespace)

  (let ((return
         (let ((c (peek-char p)))
           (cond
            ((eof-object? c)
             c)
            ((or (letter? c)
                 (special-initial? c))
             (cons 'identifier (read-identifier p)))

            ((char=? c #\#)
             (read-hash-thing p))
            ((char-numeric? c)
             (cons 'integer (read-integer p)))
            ((char=? c #\()
             (read-char p)
             'left-paren)
            ((char=? c #\))
             (read-char p)
             'right-paren)
            ((char=? c #\.)
             (read-char p)
             'dot)
            ((char=? c #\,)
             (read-char p)
             'comma)
            ((char=? c #\')
             (read-char p)
             'single-quote)
            ((char=? c #\`)
             (read-char p)
             'backquote)
            ((char=? c #\")
             (read-char p)              ; consume the opening `"'
             (let ((unquoted-chars
                    (lambda ()
                      (port->string p (lambda (c)
                                        (not (or (char=? c #\")
                                                 (char=? c #\\))))))))
               (let loop ((return (unquoted-chars)))
                 (let ((c (peek-char p)))
                   (cond
                    ((eof-object? c)
                     (string-append "End of file inside string constant -- read so far: `"
                                    return
                                    "'"))
                    ((char=? c #\")
                     (read-char p)      ;consume the closing `"'
                     (cons 'string return))
                    (else               ; we hit a backslash.
                     (read-char p)      ; consume it.
                     (loop (string-append
                            (if (char=? (peek-char p)
                                        #\\)
                                (begin
                                  (read-char p) ; consume second `\'
                                  (string-append return (string #\\)))
                              return)
                            (string (read-char p)) ; include the `"' in the string
                            (unquoted-chars)))))))))
            (else
             (cons 'unknown (string (read-char p))))))))
    (eat-whitespace)
    return))

;; Reads characters from p as long as they satisfy PREDICATE.  Also,
;; of course, stops reading when it hits end-of-file.  Returns the
;; accumulated characters as a string.
(define (port->string p predicate)
  
  ;; Appends CH to the string defined by the first USED characters in
  ;; BUFFER, and returns that string.  If BUFFER has some unused
  ;; characters, then this function simply returns BUFFER; otherwise,
  ;; it copies the characters from BUFFER into a larger buffer,
  ;; appends CH, and returns the larger buffer.

  ;; You probably want to call
  ;;
  ;; (lambda (foo)
  ;;    (make-shared-substring foo 0 (+ 1 used)))
  ;;
  ;; on the result.

  (define (my-append! buffer used ch)
    (define (larger buffer)
      (string-append buffer (make-string (+ 1 (string-length buffer)))))
    (if (= used (string-length buffer))
        (set! buffer (larger buffer)))
    (string-set! buffer used ch)
    buffer)

  (let loop ((result "")
             (chars-read 0))
    (if (or
         (eof-object? (peek-char p))
         (not (predicate (peek-char p))))
        (make-shared-substring result 0 chars-read)
      (loop (my-append! result chars-read (read-char p))
            (+ 1 chars-read)))))

(define display-tokens
  (lambda (p)
    (for-each
     (lambda (t)
       (begin
         (if (pair? t)
             (begin
               (display (car t))
               (display " ")
               (write (cdr t)))
           (display t))
         (newline)))
     (accumulate-groups p read-token))))

(define (accumulate-groups p grouper-proc)
  (let loop ((t (grouper-proc p))
             (return '()))
    (if (eof-object? t)
        (reverse return)
      (loop (grouper-proc p)
            (cons t return)))))

(define (read-expression p)
  (let ((t (read-token p)))
    (if (eof-object? t)
        t
      (cond
       ((eq? 'false t)
        #f)
       ((eq? 'true t)
        #t)
       ((eq? 'backquote t)
        (list 'quasiquote (read-expression p)))
       ((eq? 'comma t)
        (list 'unquote (read-expression p)))
       ((eq? 'dot t)
        (error "What's a dot doing here?"))
       ((eq? 'left-paren t)
        (cond
         ((char=? #\) (peek-char p))
          (read-token p)                ; consume the close paren
          '())
         (else
          (let loop ((have-we-seen-a-dot? #f)
                     (exp (read-expression p))
                     (return '()))
            (let ((c (peek-char p)))
              (cond
               ((eof-object? c)
                (reverse return))
               ((char=? #\.  c)
                (if have-we-seen-a-dot?
                    (error "Hey!  You gots two dots in one list."))
                (set! have-we-seen-a-dot? #t)
                (display "Discarding a dot: `")
                (display (read-token p)) ; consume the dot
                (display "'\n")

                (let ((last (read-expression p)))
                  (if (eof-object? last)
                      (error "Hey!  End-of-file after a dot."))
                  (let ((return (reverse return)))
                    (set-cdr! (list-tail return (- (length return) 1))
                              last)
                    (display "Consumed `")
                    (display (read-token p))
                    (display "'; that had better be a right paren")
                    (newline)
                    return)))
               ((char=? #\)  c)
                (begin
                  (read-token p)        ; consume right-paren
                  (reverse (cons exp return))))
               (else
                (loop have-we-seen-a-dot?
                      (read-expression p)
                      (cons exp return)))))))))
       ;; ((eq? 'vector-left-paren t))
       
       ((eq? 'single-quote t)
        (list 'quote (read-expression p)))
       ((not (pair? t))
        (error "Unrecognized token" t))
       (else
        (cond
         ((eq? 'character (car t))
          (cdr t))
         ((eq? 'identifier (car t))
          (string->symbol (cdr t)))
         ((eq? 'string (car t))
          (cdr t))
         ((eq? 'integer (car t))
          (cdr t))
         (else
          (error "Unrecognized token" t))))))))

(require 'pretty-print)
(if #t
    (call-with-input-string 
     "+bob + fred+ethel \"bob\" 99 #\\newline ; ignore this\n 1 ( foo bar baz ) 2 22;four\n bob 3 d ( yo cuz )"
     (lambda (p) 
       (write (accumulate-groups p read-expression))))
  (call-with-input-file 
      "eval10.scm" 
    (lambda (p) (pretty-print (accumulate-groups p read-expression)))))

