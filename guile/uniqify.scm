;; Given a list of strings, return another list of strings such that
;; --

;; each string in the returned list appears exactly once.  That is, no
;; string in the returned list is `equal?' to any other string in that
;; list.

;; each string in the input list appears once in the returned list.

;; each string in the returned list appears at least once in the input
;; list.

;; A simple way to do this would be to sort the input list, and then
;; `uniqify' it like this:

;;       (define (uniqify seq equal?)
;;         (cond
;;          ((not (pair? seq))
;;           seq)
;;          ((null? (cdr seq))
;;           seq)
;;          ((equal? (car seq)
;;                   (cadr seq))
;;           (uniqify (cdr seq) equal?))
;;          (#t
;;           (cons (car seq)
;;                 (uniqify (cdr seq)  equal?)))))

;; However, I suspect that for very large inputs, this will be very
;; slow (sorting would be slow, and so would uniqifying).  ("Premature
;; optimization is the root of all evil" -- Donald Knuth)

;; So instead, we go through the list, and add each item to a hash
;; table.  Then we run through the hash table and extract the keys.

(define (uniqify strings)
  (define stuff-into-hash! #f)
  (define hash-keys        #f)

  (let ()
    (define hash-table (make-vector 103 0))

    (set! stuff-into-hash!
          (lambda ( strings)
            (for-each 
             (lambda (str)
               (hash-set! hash-table str 1))
             strings)))

    (set! hash-keys
          (lambda ()
            (map caar (filter pair? (vector->list hash-table))))))


  (stuff-into-hash! strings)
  (hash-keys))
