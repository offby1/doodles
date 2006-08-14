(define html-quote
  (lambda args
    (define (internal-quote str)
      (let loop ((str str)
                 (result ""))
        (if (zero? (string-length str))
            result
          (let ((first (substring str 0 1)))
                  
            (loop (substring str 1 (string-length str))
                  (string-append
                   result
                   (cond
                    ((string=? first "&")
                     "&amp;")
                    ((string=? first "<")
                     "&lt;")
                    ((string=? first ">")
                     "&gt;")
                    (#t
                     first))))))))
    (apply string-append (map internal-quote args))))

(define (html-table list-of-lists)
  (define (do-rows rows first-row-is-header?)
    (define (do-one-row row header?)
      (define (do-data data)
        (define (do-one-datum datum)

          (string-append
           (if header? "<th>" "<td>")
           (html-quote datum)
           "\n"))
        
        (if (null? data)
            ""
          (string-append
           (do-one-datum (car data))
           (do-data (cdr data)))))

      (string-append
       "<tr>\n"
       (do-data row)))
    (let loop ((rows rows)
               (rows-processed 0)
               (result ""))
      (if (null? rows)
          result
        (loop (cdr rows)
              (+ 1 rows-processed)
              (string-append result
                             (do-one-row (car rows)
                                         (and
                                          first-row-is-header?
                                          (zero? rows-processed)))
                             )))))

  (string-append
   "<table border=true>\n"
   (do-rows list-of-lists #t)
   "</table>\n"))

(provide 'html)
