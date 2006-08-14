(require 'generic-read)
(require 'whitespace)
(require 'files)

(define (parse-ini fn)
  (define (sections fn)

    ;; Something like an input port, but returns strings instead of
    ;; characters, and is buffered, which means you can write a string to
    ;; it; that string becomes the next thing returned.

    (define (make-buffered-line-port port)
      
      ;; Like my-read-line, but discards blank lines and lines whose
      ;; first non-white character is a semi-colon.
      (define my-read-section-line
        (lambda (port)
            
          (let loop ((line (my-read-line port)))
            (if (eof-object? line)
                line
              (begin
                (set! line (strip-whitespace line))    
                (if (or
                     (zero? (string-length line))
                     (char=? #\; (string-ref line 0)))
                    (loop (my-read-line port))
                  line))))))

      (let ((buffer #f))
      
        (lambda (op . args)
            
          (case op
            ((read-line)
             (if buffer
                 (let ((tmp buffer))
                   (set! buffer #f)
                        
                   tmp)
               (my-read-section-line port)))
          
            ((peek-char)
             (if buffer
                 (string-ref buffer 0)
               (peek-char port)))
          
            ((write-line)
             (if buffer
                 (error "Don't write a line when there's already one in the buffer" buffer))
               
             (set! buffer (car args)))
          
            (else
             (error "Unknown operation: " op))))))
    
    ;; Reads a "section" from a Microsoft .ini file.
    ;; returns something like this:

    ;; ("[section name]\n" "a=b" "c=d" "etc=etc")

    ;; BLP must be a buffered line reader as described above.

    (define (read-section blp) 

      ;; read one line.

      ;; if it's not eof, read lines until eof or the beginning of a
      ;; section.

      ;; if it was the beginning of a section, put it back.
    
      (define (begins-section? line)
        (and (not (zero? (string-length line)))
             (char=? #\[ (string-ref line 0))))

      (if (eof-object? (blp 'peek-char))
          (blp 'peek-char)

        (let loop ((line (blp 'read-line))
                   (result '()))
          (cond
           ((eof-object? line)
            (reverse result))
     
           ((begins-section? line)
            ;; If we haven't yet read the first line of a section,
            ;; then save this line and keep reading.
            (if (null? result)
                (loop (blp 'read-line)
                      (cons line result))

              ;; Otherwise we're done.  Put back the section beginning
              ;; that we just read.
              (begin
                (blp 'write-line line)
                (reverse result))))
     
           (#t
            (loop (blp 'read-line)
                  
                  ;; only save this line if we've started saving lines.
                  (if (not (null? result))
                      (cons line result)
                    result)))))))
    
    (call-with-input-file fn
      (lambda (p)
        (let ((blp (make-buffered-line-port p)))
          (let loop ((section (read-section blp))
                     (result '()))
            (if (eof-object? section)
                (reverse result)
              (loop (read-section blp)
                    (if (null? section)
                        result
                      (cons section result))
                    )))))))

  ;; s is a list of one or more strings.
  ;; the first string begins with a left bracket and ends (except for
  ;; trailing whitespace) with a right bracket.
  ;; The other strings all have at least one equals sign.
  ;; This function returns a list whose first element is the string
  ;; inside the brackets, and whose successive elements are pairs, whose
  ;; cars are the stuff to the left of the equals sign, and whose cdrs
  ;; are the stuff to the right.
  (define (parse-section s)
    (define (strip-brackets s)

      (set! s (strip-whitespace s))

      (if (not (char=? #\[ (string-ref s 0)))
          (error "First non-white character isn't a left-bracket: " s))
      (if (not (char=? #\] (string-ref s (- (string-length s) 1))))
          (error "Last non-white character isn't a right-bracket: " s))

      (substring s 1 (- (string-length s) 1)))

    (define (parse-assignment s)
      (set! s (strip-whitespace s))
      (let ((index-of-first-equals (find-char s (lambda (c) (char=? c #\=)) #t)))
        (if (not index-of-first-equals)
            (error "String contains no equals sign: " s))
        (cons (strip-whitespace (substring s 0 index-of-first-equals))
              (strip-whitespace (substring s (+ 1 index-of-first-equals)
                                           (string-length s))))))
  
    (cons (strip-brackets (car s))
          (map parse-assignment (cdr s))))
  
  (map parse-section (sections fn)))

(define (ini->string ini)
  (define (section->string s)
    (define (assignments->string as)
      (let loop ((as as)
                 (result ""))
      (if (null? as)
          result
        (loop (cdr as)
              (string-append result (caar as)
                             "="
                             (cdar as)
                             (string #\newline))))))

    (string-append "[" (car s)
                   "]" (string #\newline)
                   (assignments->string (cdr s))))
  
  (let loop ((ini ini)
             (result ""))
    (if (null? ini)
        result
      (loop (cdr ini)
            (string-append result (section->string (car ini)))))))


(provide 'parse-ini)

(lambda ()
  (with-output-to-file "c:\\all-ini-files"
    (lambda ()
      (define (is-ini-file? fn)
        (let ((last-dot (find-char fn (lambda (c)
                                        (char=? c #\.))
                                   #f)))
          (and last-dot
               (string-ci=? ".ini"
                            (substring fn last-dot (string-length fn))))))      
      (pp (map (lambda (fn) 
                 (cons fn
                       (parse-ini fn))
                 )
               (list-files-under "c:\\" is-ini-file? #t))))))

;; rewrites the input file into a temp file.
(lambda (ini-file-name)
  (call-with-output-file (tmpnam) 
    (lambda (p) 
      (display  
       (apply string-append
	      (map
	       (lambda (s)
		 (string-append "["
				(car s)
				"]"
				(string #\newline)
				(apply string-append (map (lambda (p)
							    (string-append (car p)
									   "="
									   (cdr p)
									   (string #\newline)))
							  (cdr s)))))
	       (parse-ini ini-file-name)))       
       p))))