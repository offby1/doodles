(define kdp? #f)
(define kdp-level #f)
(define kdp-uid #f)
(define kdp-keyword #f)
(define kdp-data #f)
(define kdp->string #f)
(define kdp->object #f)
(define kdp #f)

(let ()

  (define (uid? thing)

    (if (symbol? thing)
        (set! thing (symbol->string thing)))

    (and  
     (string? thing)
     (>= (string-length thing) 4)         ; two @s, a letter, and a digit
     (char=? #\@ (string-ref thing 0))
     (memq (string-ref thing 1) '(#\I #\F)) 
     (char=? #\@ (string-ref thing (- (string-length thing)
                                      1)))
     (let ((number (make-shared-substring thing 2 (- (string-length
                                                      thing)
                                                     1))))
       (not (not (string->number number))))))

  (define kdp-rtd (make-record-type 
                   "kdp" 
                   '(
                     level              ; (and (integer? level) (not  (negative? level)))
                     uid                ; (or (uid? uid) (not (not uid)))
                     keyword            ; (symbol? keyword)
                     data               ; (or (string? data) (not (not data)))
                     )))
  (set! kdp? (record-predicate kdp-rtd))
  (set!
   kdp
   (lambda (line)
     (define level #f)
     (define uid #f)
     (define keyword #f)
     (define data #f)
     (set! level (string->number (nth-word line 0))) (set! line (string-tail line 1))
     (let ((tmp (nth-word line 0)))
       (if (uid? tmp)
           (begin
             (set! uid (string->symbol tmp)) (set! line (string-tail line 1)))))
     (set! keyword (string->symbol (nth-word line 0))) (set! line (string-tail line 1))
     (let ((tmp (nth-word line 0)))
       (define (whack-final-newline string)
         (let ((l (string-length string)))
           (if (and
                (positive? l)
                (char=? #\newline (string-ref string (- l 1))))
               (make-shared-substring string 0 (- l 1))
             string)))
       (if (positive? (string-length tmp))
           (set! data
                 (if (uid? tmp)
                     (string->symbol tmp)
                   (whack-final-newline line)))))

     ((record-constructor kdp-rtd) level uid keyword data)))

  (set! kdp-level   (lambda (kdp) ((record-accessor kdp-rtd 'level  ) kdp)))
  (set! kdp-uid     (lambda (kdp) ((record-accessor kdp-rtd 'uid    ) kdp)))
  (set! kdp-keyword (lambda (kdp) ((record-accessor kdp-rtd 'keyword) kdp)))
  (set! kdp-data    (lambda (kdp) ((record-accessor kdp-rtd 'data   ) kdp)))

  (set!
   kdp->string
   (lambda (kdp)
     (string-append
      (number->string (kdp-level kdp))
      (let ((tmp (kdp-uid kdp)))
        (if tmp
            (string-append " " (symbol->string tmp))
          ""))
      " "
      (symbol->string (kdp-keyword kdp))
      (let ((tmp (kdp-data kdp)))
        (if tmp
            (string-append " " tmp)
          "")))))
                 
  (set!
   kdp->object
   (lambda (kdp)
     (let ((data (kdp-data    kdp))
           (kw   (kdp-keyword kdp))
           (uid  (kdp-uid     kdp))
           (return '()))
       (if data
           (set! return (list data)))
       
       (set! return (cons kw return))
       (if uid
           (set! return (cons uid return)))
       return))))
