(define gedcom #f)
(define gedcom? #f)
(define gedcom-add-section! #f)
(define gedcom-sections #f)
(define gedcom->string #f)
(define gedcom->object #f)

(let ()
  (define gedcom-rtd (make-record-type "gedcom" '(sections)))
  (set!
   gedcom? (record-predicate gedcom-rtd))
  (set!
   gedcom (lambda () ((record-constructor gedcom-rtd) '())))
  (set!
   gedcom-add-section!
   (lambda (g thing)
     (if (not (section? thing))
         (error "Arg should be a section, but is" thing))
     ((record-modifier gedcom-rtd 'sections)
      g
      (cons 
       thing 
       ((record-accessor gedcom-rtd 'sections) g)))))

  (set!
   gedcom-sections
   (lambda (g)
     (let ((tmp ((record-accessor gedcom-rtd 'sections) g)))
       (if (null? tmp)
           (error "This gedcom has no sections"))
       (reverse tmp))))

  (set!
   gedcom->string
   (lambda (g)
     (apply
      string-append
      (map section->string (gedcom-sections g)))))

  (set!
   gedcom->object
   (lambda (g)
     (map section->object (gedcom-sections g)))))


(define section #f)
(define section? #f)
(define section-add-gedcom! #f)
(define section->string #f)
(define section-kdp #f)
(define section->object #f)

(let ()
  ;; kdp stands for `keyword-data-pair'.

  (define section-rtd (make-record-type "section" '(kdp gedcoms)))
  (set! section? (record-predicate section-rtd))
  (set!
   section (lambda (kdp)
             (if (not (kdp? kdp))
                 (error "Arg must be a kdp, but is" kdp))
             ((record-constructor section-rtd) kdp '())))
  (set!
   section-add-gedcom!
   (lambda (s thing)
     (if (not (gedcom? thing))
         (error "Arg should be a gedcom, but is" thing))

     ((record-modifier section-rtd 'gedcoms)
      s
      (cons 
       thing 
       ((record-accessor section-rtd 'gedcoms) s)))))

  (set! section-kdp (record-accessor section-rtd 'kdp))
  
  (set!
   section->string
   (lambda (s)
     (apply
      string-append
      (cons
       (kdp->string (section-kdp s))
       (cons
        (string #\newline)
        (map
         gedcom->string
         ((record-accessor section-rtd 'gedcoms) s)))))))
  (set!
   section->object
   (lambda (s)
     (cons (kdp->object (section-kdp s))
           (map gedcom->object ((record-accessor section-rtd 'gedcoms)
                                s))))))

