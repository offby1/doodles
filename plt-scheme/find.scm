(define-struct directory (name entries) #f)
(define example `(".bashrc" ,(make-directory "Itineraries" '("mexico" "new york")) "Phones"))
(define output `("./.bashrc" "./Itineraries" "./Itineraries/mexico" "./Itineraries/new york" "./Phones"))

(define (dir->list d)
  (cons (directory-name d)
        (map (lambda (entry)
               (string-append (directory-name d) "/" entry))
             (directory-entries d))))

(require (lib "trace.ss"))
(define (flatten current-dir thing)
  (cond
   ((string? thing)
    (list (string-append current-dir "/" thing)))
   ((list? thing)
    (apply append (map (lambda (entry)
           (flatten current-dir entry))
         thing)))
   ((directory? thing)
    (map (lambda (entry)
           (string-append current-dir "/" entry))
         (dir->list thing)))
   (#t
    (error "Ain't no thang" thing))))
(trace flatten)

(flatten "." example)
