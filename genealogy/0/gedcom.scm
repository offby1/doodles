(require 'generic-read)
(require 'filter)

(load "strings.scm")
(load "family.scm")
(load "person.scm")
(load "uid.scm")

(define (file->gdb filename)

  (define LINES
    (let ()

      ;; Read individual lines into a list
      (define gedcom-lines
        (reverse
         (let ((return #f))
           (display "Reading `")
           (display filename)
           (display "' ...") (force-output)
           (set!
            return
            (call-with-input-file
                filename
              (lambda (port)
                (generic-reader port
                                my-read-line
                                (lambda (x) #f)
                                (lambda (x) x)
                                '()
                                (lambda (a b) (cons b a))))))
           (display "done")
           (newline)
           return)))

      (define (split-data data boundary-first-word boundary-last-word)

        (define (lines->datum strings boundary-first-word)
          (let loop ((strings strings)
                     (result '()))
            (if (or (null? strings)
                    (and
                     (not (null? result))
                     (string=? (first-word (car strings)) boundary-first-word)))
                (reverse result)
              (loop (cdr strings)
                    (cons (car strings)
                          result)))))

        (display "Splitting lines ...")
        (let ((return
               (let loop ((data data)
                          (result '()))
  
                 (if (null? data)
                     result
                   (loop (cdr data)
                         (let ((current-line (car data)))
                           (if (and (string=? (first-word current-line) boundary-first-word)
                                    (string=? (last-word  current-line) boundary-last-word))
                               (cons (lines->datum data boundary-first-word)
                                     result)
                             result)))))))
          (display "done")
          (newline)
          return))

      (append (split-data gedcom-lines "0" "INDI")
              (split-data gedcom-lines "0" "FAM"))))

  (let* ((PEOPLE-LINES
          (filter
           (lambda (string-list)
             (string=? (nth-word 2 (car string-list))
                       "INDI"))
           LINES) )
         (PEOPLE
          (begin
            (display "Creating individuals ")(force-output)
            (let ((return
                   (map
                    (lambda (lines)
                      (display ".") (force-output)
                      (person-ctor lines))
                    PEOPLE-LINES)))
        
              (display "done") (newline)
              return))))

    (define FAMILIES
      (begin
        (display "Creating and linking families ...") (force-output)    
        (let ((return (map
                       (lambda (lines)
                         (display ".") (force-output)
                         (family-ctor lines PEOPLE-LINES))
                       (filter
                        (lambda (string-list)
                          (string=? (nth-word 2 (car string-list))
                                    "FAM"))
                        LINES))))
          (display "done") (newline)
          return)))

    (cons PEOPLE FAMILIES)))
