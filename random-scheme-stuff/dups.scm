(require 'sort)
(define (report-duplicates strings)

  (define (omitting-initial elt seq)
    (if (null? seq)
        seq
      (if (string-ci=? elt (car seq))
          (omitting-initial elt (cdr seq))
        seq)))

  (set! strings (sort strings string-ci<?))
  
  (let loop ((strings strings)
             (results '()))
    (if (or (null? strings)
            (null? (cdr strings)))
        results
      (if (string-ci=? (car strings)
                       (cadr strings))
          (loop (omitting-initial (car strings) strings)
                (cons (car strings)
                      results))
        (loop (cdr strings)
              results)))))

