(module hand mzscheme
  (require  (all-except (lib "list.ss" "srfi" "1") remove)
            (rename (lib "list.ss") remove remove)
            "constants.ss"
            "exceptions.ss"
            (lib "trace.ss"))
  (provide
   hand-ref
   hand-length
   without-card
   hand->list
   (rename my-make-hand make-hand))

  (print-struct #t)
  
  (define-struct hand (cards) #f)

  (define (my-make-hand cardv)
    (let ((desired-length (/ *deck-size*
                             (length *seats*))))
      (when (not (= desired-length (vector-length cardv)))
        (raise-bridge-error 'make-hand (format "vector of length ~a" desired-length) cardv)))
    (make-hand (vector->list cardv)))

  (define (hand-ref h K)
    (list-ref (hand-cards h) K))
  
  (define (hand-length h)
    (length (hand-cards h)))

  (define (without-card c h)
    (make-hand (remove c (hand-cards h))))
  
  (define (hand->list h)
    (hand-cards h))
  )