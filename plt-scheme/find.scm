(define-struct directory (name entries) #f)
(define example `(".bashrc" ,(make-directory "Itineraries" '("mexico" "new york")) "Phones"))
(define output `("./.bashrc" "./Itineraries" "./Itineraries/mexico" "./Itineraries/new york" "./Phones"))

(define (pfx d f)
  (string-append d "/" f))

(define (mappfx d seq)
  (map (lambda (entry)
         (pfx  d entry))
        seq))

(define (dir->list d)
  (cons (directory-name d)
        (mappfx (directory-name d) (directory-entries d))))

(require (lib "trace.ss"))
(define (flatten current-dir thing)
  (cond
   ((string? thing)
    (list (pfx  current-dir thing)))
   ((list? thing)
    (apply append (map (lambda (entry)
           (flatten current-dir entry))
         thing)))
   ((directory? thing)
    (mappfx  current-dir (dir->list thing)))
   (#t
    (error "Ain't no thang" thing))))
(trace flatten)

(flatten "." example)
