(module misc mzscheme
  (provide
   swap!
   level-or-zero
   append-map-at-most
   take-at-most
   group-by)
  (require "call.ss")
  
  (define-syntax swap!
    (syntax-rules ()
      ((_ a b)
       (let ((tmp a))
         (set! a b)
         (set! b tmp)))))

  (define (level-or-zero thing)
    (if (and thing
             (bid? thing)) (level thing)
      0))

  (define (take-at-most seq n)
    (fold-at-most cons '() n seq))

  (define (fold-at-most kons knil max seq)
    (let loop ((seq seq)
               (max max)
               (result knil))
      (if (or (null? seq)
              (zero? max))
          (reverse result)
        (loop (cdr seq)
              (- max 1)
              (kons (car seq)
                    result)))))

  (define (map-at-most proc n seq)
    (fold-at-most (lambda (first rest)
                    (cons (proc first)
                          rest))
                  '()
                  n
                  seq))

  (define (append-map-at-most proc n seq)
    (apply append (map-at-most proc n seq)))
  (define (group-by N seq)
    (define (inner seq complete-chunks chunk chunk-length)
      (if (null? seq)
          (reverse (cons (reverse chunk) complete-chunks))
        (let ((complete? (= chunk-length N)))
          (inner (cdr seq)
                 (if complete? (cons (reverse chunk) complete-chunks) complete-chunks)
                 (if complete? (list (car seq))                       (cons (car seq) chunk))
                 (if complete? 1                                      (add1 chunk-length))
                 ))))
    (inner seq '() '() 0)
    )
  
  
  )
