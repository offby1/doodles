(require 'generic-read)
(load "strings.scm")

(define gdb #f)

(define x
  (lambda ()
    (define parse-gedcom #f)
    (define parse-section #f)

    (define lines 

      ((lambda (filename)
         (reverse
          (let ((return #f))
            (display "Reading `")
            (display filename)
            (display "' ... ") (force-output)
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
       "/home/offby1/me.ged"))

    (define (line-level line)
      (let ((tmp (string->number (nth-word line 0))))
        (if (not tmp)
            (error "Line must begin with a number" line))
        (if (not (and (not (negative? tmp))
                      (integer? tmp)))
            (error "Line must begin with a non-negative integer" line))
        tmp))

  
  
    ;; A gedcom has a non-negative integer level, and consists of 1 or
    ;; more sections of that level.

    ;; Each section consists of a line that begins with the level, and 0
    ;; or more gedcoms of the next higher level.

    (set!
     parse-gedcom
     (lambda (level)
       (let ((return (gedcom)))
         (let loop ((s (parse-section level)))
           (gedcom-add-section! return s)
           (if (or
                (null? lines)
                (< (line-level (car lines))
                   level))
               return
             (loop (parse-section level)))))))

    (set!
     parse-section
     (lambda (level)

       (let ((return (section (kdp (car lines)))))

         (display ".") (force-output)
         (set! lines (cdr lines))

         (let loop ()
           (if (or
                (null? lines)
                (<= (line-level (car lines))
                    level))
               return
             (begin
               (section-add-gedcom! return (parse-gedcom (+ 1 level)))
               (loop)))))))
    (display "Parsing file ... ") (force-output)
    (set! gdb (parse-gedcom 0))
    (display "done") (newline)
    (gedcom->object gdb)))
