(require 'filter)
(load "gedcom.scm")

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
          (let ((birth-family (person-get-birth-family person)))
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
    (apply string-append
           (append
            (map 
             person->gedcom 
             accumulated-people)
            (map 
             family->gedcom
             accumulated-families))))
  (define gdb (file->gdb "/home/work/gw-2-05/distribution/gw/me.ged"))
  (define eric (list-ref (reverse (car gdb)) 3))
  (display (br eric))
  (newline))
