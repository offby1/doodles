;; some functions to work with Guile's (version 1.3.4) built-in hash
;; tables

(define (hash-keys table)
  (let loop ((kv-pairs (apply append (vector->list table)))
             (result '()))
    (if (null? kv-pairs)
        result
      (loop (cdr kv-pairs)
            (if (member (caar kv-pairs)
                        result)
                result
              (cons (caar kv-pairs)
                    result))))))
