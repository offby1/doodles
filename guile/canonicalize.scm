(define (canonicalize fn)
  (define (is-absolute? fn)
    (and (not (zero? (string-length fn)))
         (char=? #\/ (string-ref fn 0))))

  (define (reconstruct-non-UNC l)
    (let loop ((l l)
               (result ""))
      (if (null? l)
          result
        (loop (cdr l)
              (string-append result
                             (if (or
                                  (string=? result "")
                                  (string=? result "/"))
                                 ""
                               "/")
                             (car l))))))

  (define (parse-filename fn)
    (let ((absolute (is-absolute? fn)))

      ;; skip leading slash
      (if absolute (set! fn (make-shared-substring fn 1)))

      (reverse
       (let loop ((fn fn)
                  (result (if absolute
                              '("/")
                            '())))
         (if (zero? (string-length fn))
             result
           (let ((index-of-end-of-component (string-index fn #\/))
                 (this-component #f))
             (set! this-component
                   (if index-of-end-of-component
                       (make-shared-substring fn 0 index-of-end-of-component)
                     fn))
             (loop (if index-of-end-of-component
                       (make-shared-substring fn (+ 1 index-of-end-of-component))
                     "")
                   (if (not (zero? (string-length this-component)))
                       (cons this-component result)
                     result))))))))
  (define (parse-possible-UNC-name fn)
    (if (and
         (< 1 (string-length fn))
         (char=? #\/ (string-ref fn 0))
         (char=? #\/ (string-ref fn 1)))
        (begin
          (set! fn (make-shared-substring fn 2))
          (let* ((parsed (parse-filename fn))
                 (server (car parsed))
                 (share  (cadr parsed))
                 (rest   (cddr parsed)))
            ;; now what?
            'hmm))
      ;; it's not a UNC name; just parse it normally.
      (parse-filename fn)))

  (define (deal-with-dots components)
    (let loop ((components components)
               (result '()))
      (if (null? components)
          (reverse result)
        (loop (cdr components)
              (cond
               ((string=? ".." (car components))
                (cond

                 ((or
                   (null? result)
                   (string=? "." (car result)))
                  (list ".."))

                 ;; parent of root is still root
                 ((string=? "/" (car result))
                  result)

                 ((string=? ".." (car result))
                  (cons ".." result))

                 ((null? (cdr result))
                  (list "."))

                 (#t
                  (cdr result))))

               ((string=? "." (car components))
                result)
               (#t
                (cons (car components)
                      result)))))))
  
  (reconstruct-non-UNC
   (let ((parsed (deal-with-dots (parse-filename fn))))
     (if (is-absolute? fn)
         parsed
       (deal-with-dots (append (parse-filename (getcwd))
                               parsed))))))