(require 'filter)

(define person-mother #f)
(define person-father #f)
(define person-full-siblings #f)
(define person-children #f)
(define person-summary #f)
(define person-birth-family #f)

(let ()
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

    (define (person-birth-family person-symbol)
      (lookup (sloppy-cadr (sloppy-assq 'FAMC (lookup person-symbol)))))

    (let ((birth-family (person-birth-family person-symbol)))
      (sloppy-cadr (sloppy-assq
                    (if male? 'HUSB 'WIFE)
                    birth-family))))
  
  (set! person-mother (lambda (p)
                        (parent #f p)))
  (set! person-father (lambda (p)
                        (parent #t p)))
  (set!
   person-children
   (lambda (psym)
     (apply 
      append
      (map (lambda (famsym)
             (map cadr
                  (filter
                   (lambda (p)
                     (eq? (car p)
                          'CHIL))
                   (lookup famsym))))
           (let* ((person (lookup psym))
                  (spousal-families
                   (filter (lambda (p)
                             (eq? (car p)
                                  'FAMS)) 
                           person)))
             spousal-families
             (map cadr spousal-families))))))
  
  (set!
   person-full-siblings
   (lambda (psym)
     (filter
      (lambda (sibling-symbol)
        (and sibling-symbol (not (eq? sibling-symbol psym))))
      (map
       sloppy-cadr
       (filter
        (lambda (p)
          (eq? 'CHIL (car p)))
        (lookup (sloppy-cadr (sloppy-assq 'FAMC (lookup psym)))))))))

  (set! 
   person-summary
   (lambda (psym)
     (sloppy-cadr (assq 'NAME (lookup psym)))))
  
  (set!
   person-birth-family
   (lambda (psym)
     (sloppy-cdr (sloppy-assq 'FAMC psym)))))

(begin
  (define (br person)
    (define in-progress '())
    (define accumulated-people '())
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
    
      (if 
          (memq person in-progress)
          ;;(member (person-uid person) in-progress)
          '()

        (let ((ma (person-mother person))
              (pa (person-father person))
              (kids (person-children person)))

          ;;(set! in-progress (cons (person-uid person) in-progress))
          (set! in-progress (cons person in-progress))

          (display "Processing ") (display (person-summary person))
          (display "; ")
          (display (if ignore-parents? "" "not ")) (display "ignoring parents")
          (newline)

          (set! accumulated-people (cons person accumulated-people))
          (let ((birth-family (person-birth-family (lookup person))))
            
            (if (and birth-family
                     (not (memq birth-family accumulated-families)))
                (set! accumulated-families (cons birth-family
                                                 accumulated-families))))

          (cons (cons person distance)
                (append
                 (if ignore-parents?
                     '()
                   (append
                    (maybe-recurse ma ignore-parents?) 
                    (maybe-recurse pa ignore-parents?))) 
                 (maybe-flatmap (lambda (kid)
                                  (maybe-recurse kid #t)) kids))))))
  
    (internal-br person #f 0)
    accumulated-people
    ;;(apply string-append (append (map person->gedcom accumulated-people) (map family->gedcom accumulated-families)))
    ))
