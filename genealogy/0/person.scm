(require 'record)

(define person-ctor #f)
(define person? #f)
(define person-uid #f)
(define person-sex #f)
(define person-set-birth-family!  #f)
(define person-get-birth-family   #f)
(define person-add-other-family!  #f)
(define person-get-other-families #f)
(define person-mother #f)
(define person-father #f)
(define person-children #f)
(define person-summary #f)
(define person->gedcom #f)

(let ()
  
  (define person-rtd (make-record-type "person" '(uid gedcom-lines birth-family other-families)))
  (set! person? (record-predicate person-rtd))
  (set! person-ctor
        (lambda (lines)
          (if (not (and (list? lines)
                        (string? (car lines))))
              (error "First arg must be a list of strings, but is" lines))
          (if (not (string->number (nth-word 0 (car lines))))
              (error "First word of first line must be a number" lines))
      
      
          (let* ((tmp ((record-constructor person-rtd '(gedcom-lines)) lines))
                 (uid (nth-word 1 (car lines))))
            (if (not (uid? uid))
                (error "Second word of first line must be a uid" lines))  
            ((record-modifier person-rtd 'uid) tmp uid)
            ((record-modifier person-rtd 'birth-family) tmp #f)
            ((record-modifier person-rtd 'other-families) tmp '())

            tmp)))
  (set! person-uid (record-accessor person-rtd 'uid))
  (set! person-sex
        (lambda (person)
          (let ((sex-lines
                 (filter (lambda (line)
                           (string=? (nth-word 1 line)
                                     "SEX")))
                 ((record-accessor 'person-rtd 'gedcom-lines) person)))
            (if (> (length sex-lines)
                   1)
                (error "Too many sex lines" lines))
            (let ((sex-string (nth-word 2 (car sex-lines))))
              (cond
               ((null? sex-lines) 'unknown)
               ((string= sex-string "F") 'female)
               (t 'male))))))
  (set! person-set-birth-family! 
        (lambda (person bf)
          (if (not (family? bf))
              (error "Second arg must be a family, but is" bf))
          
          ((record-modifier person-rtd 'birth-family) person bf)))
  (set! person-get-birth-family   (record-accessor person-rtd 'birth-family))
  (set! person-get-other-families
        (lambda (person)
          (reverse ((record-accessor person-rtd 'other-families) person))))
  (set! person-add-other-family!
        (lambda (person of)
          (if (not (family? of))
              (error "Second arg must be a family, but is" of))
          (let* ((old ((record-accessor person-rtd 'other-families) person)))
            ((record-modifier person-rtd 'other-families) person (cons of old)))))
  (set! person-mother
        (lambda (person)
          (let ((birth-family (person-get-birth-family person)))
            (and birth-family
                 (family-wife birth-family)))))
  (set! person-father
        (lambda (person)
          (let ((birth-family (person-get-birth-family person)))
            (and birth-family
                 (family-husband birth-family)))))
  (set! person-children
        (lambda (person)
          (let loop ((other-families (person-get-other-families person))
                     (result '()))
            (if (null? other-families)
                result
              (loop (cdr other-families)
                    (append
                     (family-children (car other-families))
                     result))))))
  
  (let ()
    (define (whack-final-newline str)
      (let ((l (string-length str)))
        (if (char=? #\newline (string-ref str (- l 1)))
            (make-shared-substring str 0 (- l 2))
          str)))

    (set! 
     person-summary 
     (lambda (p)
       (whack-final-newline
        (list-ref 
         ((record-accessor person-rtd 'gedcom-lines) p)
         1)))))

  (set!
   person->gedcom
   (lambda (p)
     (apply string-append ((record-accessor person-rtd 'gedcom-lines) p)))))

