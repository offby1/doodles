(define family-ctor #f)
(define family? #f)
(define family-uid #f)
(define family-wife #f)
(define family-husband #f)
(define family-children #f)
(define family-summary #f)
(define family->gedcom #f)

(let ()
  
  (define (lookup uid person-lines)
    (let ((tmp
           (filter (lambda (person)
                     (uid=? (person-uid person)
                            uid))
                   person-lines)))
      (if (= (length tmp)
             1)
          (car tmp)
        (error "Not exactly one person with uid" uid tmp))))

  (define family-rtd 
    (make-record-type
     "family" 
     '(uid gedcom-lines wife husband children)))
  (set! family? (record-predicate family-rtd))
  (set! family-uid (record-accessor family-rtd 'uid))
  (set! family-wife (record-accessor family-rtd 'wife))
  (set! family-husband (record-accessor family-rtd 'husband))
  (set! family-children
        (lambda (f)
          (reverse ((record-accessor family-rtd
                                     'children) f))))
  (set!
   family-ctor
   (lambda (lines people-lines)
     (if (not (and (list? lines)
                   (string? (car lines))))
         (error "First arg must be a list of strings, but is" lines))
     (if (not (string->number (nth-word 0 (car lines))))
         (error "First word of first line must be a number" lines))

     (let* ((tmp ((record-constructor family-rtd '(gedcom-lines)) lines))
            (uid (nth-word 1 (car lines)))
            (sex 'unknown))
       (if (not (uid? uid))
           (error "Second word of first line must be a uid" lines))  
       
       ;;(display uid) (display " ") (force-output)
       
       ((record-modifier family-rtd 'uid) tmp uid)
       (let ((husband-lines (filter (lambda (str) (string=? (nth-word 1 str) "HUSB")) lines))
             (wife-lines    (filter (lambda (str) (string=? (nth-word 1 str) "WIFE")) lines))
             (children-lines(filter (lambda (str) (string=? (nth-word 1 str) "CHIL")) lines)))
         (if (or (> (length husband-lines) 1)
                 (> (length wife-lines)    1))
             (error "Too many spouses in this family" lines))
         
         (if (not (null? wife-lines))
             (let* ((wife-uid (nth-word 2 (car wife-lines)))
                    (wife (lookup wife-uid people-lines)))
               ;;(display "wife ") (display wife-uid) (display " ") (force-output)
               ((record-modifier family-rtd 'wife) 
                tmp 
                wife)
               ;; now modify the wife by inserting this family into
               ;; her other-families
               (person-add-other-family! wife tmp)))

         (if (not (null? husband-lines))
             (let* ((husband-uid (nth-word 2 (car husband-lines)))  
                    (husband (lookup husband-uid people-lines)))
               ;;(display "husband ") (display husband-uid) (display " ") (force-output)
               ((record-modifier family-rtd 'husband) 
                tmp 
                husband)
               (person-add-other-family! husband tmp)))
         
         ((record-modifier family-rtd 'children)
          tmp
          (let loop ((children-lines children-lines)
                     (return '()))
            (if (null? children-lines)
                return
              (loop (cdr children-lines)
                    (cons
                     (let* ((child-uid (nth-word 2 (car children-lines)))
                            (child (lookup child-uid people-lines)))
                       ;;(display child-uid) (display " ") (force-output)
                       (person-set-birth-family! child tmp)
                       child)
                     return))))))
       
       tmp)))
  (set! family-summary
        (lambda (f)
          (let ((husband (family-husband f))
                (wife    (family-wife    f)))
            (string-append
             "The family of "
             (if husband (person-summary husband) "?")
             " and "
             (if wife    (person-summary wife)    "?")))))
  (set!
   family->gedcom
   (lambda (f)
     (apply string-append ((record-accessor family-rtd 'gedcom-lines) f)))))
