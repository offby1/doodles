(require 'filter)

(define person-mother #f)
(define person-father #f)
(define person-full-siblings #f)
(define person-children #f)
(define person-summary #f)
(define person-birth-family #f)

(let ()

  (define (person-details psym) (sloppy-list-ref (maybe-lookup psym) 2))
  (define family-details #f)

  (define (sloppy-list-ref obj k)
    (and obj
         (list-ref obj k)))

  (define (sloppy-assq obj alist-or-false)
    (and alist-or-false
         (assq obj alist-or-false)))
  
  (define (sloppy-car p)
    (and p (car p)))
  
  (define (sloppy-cdr p)
    (and p (cdr p)))
  
  (define (sloppy-cadr p)
    (sloppy-car (sloppy-cdr p)))
  
  (define (parent male? person-symbol)
    
    (sloppy-cadr 
     (sloppy-assq
      (if male? 'HUSB 'WIFE)
      (family-details (person-birth-family person-symbol)))))

  (set! family-details person-details)

  (set! person-mother (lambda (p) (parent #f p)))
  (set! person-father (lambda (p) (parent #t p)))

  (set!
   person-birth-family
   (lambda (person)
     (maybe-lookup
      (sloppy-cadr
       (sloppy-assq
        'FAMC
        (person-details person))))))

  (set!
   person-children
   (lambda (psym)
     (apply 
      append
      (map 
       (lambda (spousal-family-uid)
         (map cadr
              (filter (lambda (p) (eq? (car p) 'CHIL))
                      (family-details spousal-family-uid))))
       (map cadr
            (filter (lambda (p) (eq? (car p) 'FAMS)) 
                    (person-details psym)))))))
  
  (set!
   person-full-siblings
   (lambda (psym)
     (filter
      (lambda (sibling-symbol)
        (and sibling-symbol (not (eq? sibling-symbol psym))))
      (map
       sloppy-cadr
       (filter
        (lambda (p) (eq? 'CHIL (car p)))
        (person-details
         (sloppy-cadr
          (sloppy-assq
           'FAMC
           (person-details psym)))))))))

  (set! 
   person-summary
   (lambda (psym)
     (sloppy-cadr (assq 'NAME (person-details psym))))))

(begin
  (define (br psym)

    (define accumulated-people   '())
    (define accumulated-families '())

    (define (internal-br person ignore-parents? distance)
      (define (maybe-recurse p ignore-parents?) 
        (if p
            (internal-br p ignore-parents? (+ 1 distance))
          '()))
    
      (define (maybe-flatmap proc list)
        (if list
            (flatmap proc list)
          '()))
    
      ;; Don't process this fellow if we have done so already
      (if 
          (memq person accumulated-people)
          '()

        (let ((ma (person-mother person))
              (pa (person-father person))
              (kids (person-children person)))

          (display "Processing ") (display (person-summary person))
          (if (not ignore-parents?) (display "(and parents)"))
          (display "; ")
          (newline)

          (set! accumulated-people (cons person accumulated-people))

          (let ((birth-family (person-birth-family person)))
            
            (if (and birth-family
                     (not (memq birth-family accumulated-families)))
                (set! accumulated-families (cons birth-family
                                                 accumulated-families))))

          (cons (cons person distance)
                (append
                 (if ignore-parents?
                     '()
                   (append
                    (maybe-recurse (maybe-lookup ma) ignore-parents?) 
                    (maybe-recurse (maybe-lookup pa) ignore-parents?))) 
                 (maybe-flatmap (lambda (kid)
                                  (maybe-recurse (maybe-lookup kid) #t)) kids))))))
  
    (internal-br (maybe-lookup psym) #f 0)

    (cons
     ;;make-gedcom
     accumulated-people
     accumulated-families)))
