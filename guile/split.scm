;; given a string full of interesting data, and another string that
;; contains the characters that you consider delimiters, this function
;; returns a list of strings that are the result of splitting the data
;; string at the delimiters.

;; Unfortunately this procedure isn't portable, as it relies on
;; with-input-from-string and read-delimited, both of which are
;; present in GNU Guile 1.3.4, but probably not in other Scheme
;; implementations.

;; (split "4/16/2000" "/") => ("4" "16" "2000")

(define (split string delimiters)
  ;; (display string)
  ;; (newline)
  (with-input-from-string
   string
   (lambda ()
     (let loop ((datum (read-delimited delimiters))
                (results '()))
       (if (eof-object? datum)
           (reverse results)
         (loop (read-delimited delimiters)
               (if (not (zero? (string-length datum)))
                   (cons datum results)
                 results)))))))

(provide 'split)