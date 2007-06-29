(module num-string-commas mzscheme
(require
  (planet "fmt.ss" ("ashinn" "fmt.plt" 1 0)))

(provide num-string-commas)

(define (num-string-commas n)
  (fmt #f (num/comma n)))

)
