(require 'filter)

(define (br person)
  (define in-progress '())

  (define (internal-br person ignore-parents?)
    (define (maybe-recurse p ignore-parents?) 
      (if p
          (cons p (internal-br p ignore-parents?))
        '()))
    
    (define (maybe-flatmap proc list)
      (if list
          (flatmap proc list)
        '()))
    
    (if (or 
         (not (person? person))
         (member (person-uid person)
                 in-progress))
        '()

      (let ((ma (person-mother person))
            (pa (person-father person))
            (kids (person-children person)))
        (set! in-progress (cons (person-uid person) in-progress))
        (display "Processing ") (display (person-summary person))
        (display "; ")
        (display (if ignore-parents? "" "not ")) (display "ignoring parents")
        (newline)

        (append
         (if ignore-parents?
             '()
           (append
            (maybe-recurse ma ignore-parents?) 
            (maybe-recurse pa ignore-parents?))) 
         (maybe-flatmap (lambda (kid)
                          (maybe-recurse kid #t)) kids)))))
  
  (internal-br person #f))
