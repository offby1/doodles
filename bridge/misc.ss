(module misc mzscheme
  (provide
   append-map-at-most
   group-by
   hash-table-increment!
   level-or-zero
   most-common
   swap!
   take-at-most
   )
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
  
  (define (hash-table-increment! h k)
    (let ((v (hash-table-get h k (lambda () 0))))
      (hash-table-put! h k (add1 v))))

  (define (most-common seq)
    (define ht (make-hash-table))
    (define best-pair-so-far '(#f . 0))
    (when (null? seq)
      (raise-type-error 'most-common "non-empty sequence" seq))
    (for-each 
     (lambda (item) (hash-table-increment! ht item))
     seq)
    (hash-table-for-each 
     ht 
     (lambda (k v) 
       (when (> v (cdr best-pair-so-far))
         (set! best-pair-so-far (cons k v)))))
    (car best-pair-so-far))
  )
