;; Crude replacement for html-table, intended for those who don't have
;; a web browser.
(define (hacky-table list-of-lists)
  (let ((left-padding 0))
    (define (do-rows rows)
      (define (do-one-row row)
        (define (do-data data)
          (define (do-one-datum datum)
            (string-append
             (make-string (- left-padding (string-length datum)) #\space)
             datum
             ))
        
          (if (null? data)
              ""
            (string-append
             (do-one-datum (car data))
             (do-data (cdr data)))))

        (string-append
         (do-data row)
         "\n"))
      (let loop ((rows rows)
                 (rows-processed 0)
                 (result ""))
        (if (null? rows)
            result
          (loop (cdr rows)
                (+ 1 rows-processed)
                (string-append result
                               (do-one-row (car rows)))))))
    (set! left-padding (+ 3 (apply max (map string-length (car list-of-lists)))))
    (do-rows list-of-lists)))
