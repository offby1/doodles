;; Read a gedcom file, and define some functions that examine it.
;; The file's data itself is isolated in the environment created by
;; the `let', so that the only way to manipulate the data is via the
;; functions.

;; ... of course, as the code now stands, the functions return data
;; from the actual database, as opposed to a copy; that means that if
;; you modify the returned data, you've screwed up the database.  This
;; is a bug.

(require 'generic-read)
(load "strings.scm")

(define lookup #f)

(let ()
  (define gdb #f)
  (define make-gedcom #f)
  (define make-section #f)

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
    (let ((tmp (string->number (nth-word 0 line))))
      (if (not tmp)
          (error "Line must begin with a number" line))
      (if (not (and (not (negative? tmp))
                    (integer? tmp)))
          (error "Line must begin with a non-negative integer" line))
      tmp))

  (set!
   lookup
   (lambda (symbol)
     (define (internal-lookup symbol gdb)
       (if (null? gdb)
           #f
         (let ((this-section (car gdb)))
           (if (and (>= (length this-section) 3)
                    (eq? (cadr this-section)
                         symbol))
               (caddr this-section)
             (internal-lookup symbol (cdr gdb))))))
     (internal-lookup symbol gdb)))
  
  ;; A gedcom has a non-negative integer level, and consists of 1 or
  ;; more sections of that level.

  ;; Each section consists of a line that begins with the level, and 0
  ;; or more gedcoms of the next higher level.

  (set!
   make-gedcom
   (lambda (level)
     (let loop ((sections '()))
       (if (or
            (null? lines)
            (< (line-level (car lines))
               level))
           (reverse sections)
         (loop (cons (make-section level)
                     sections))))))

  (set!
   make-section
   (lambda (level)

     (define (make-data-from-string string)

       (define (is-uid? thing)
         (and  
          (string? thing)
          (>= (string-length thing) 4)  ; two @s, a letter, and a digit
          (char=? #\@ (string-ref thing 0))
          (memq (string-ref thing 1) '(#\I #\F)) 
          (char=? #\@ (string-ref thing (- (string-length thing)
                                           1)))
          (let ((number (make-shared-substring thing 2 (- (string-length
                                                           thing)
                                                          1))))
            (not (not (string->number number))))))

       (let* ((w1 (nth-word 1 string))
              (keyword-first? (not (is-uid? w1)))
              (keyword
               (string->symbol (nth-word
                                (if keyword-first?
                                    1
                                  2)
                                string)))
              (data
               (if keyword-first?
                   (cond
                    ((memq keyword '(HUSB WIFE CHIL FAMC FAMS))
                     (string->symbol (nth-word 2 string)))
                    (#t
                     (let ((rest (string-tail string 2)))
                       (define (whack-final-newline string)
                         (let ((l (string-length string)))
                           (if (and
                                (positive? l)
                                (char=? #\newline (string-ref string (- l 1))))
                               (make-shared-substring string 0 (- l 1))
                             string)))
                       (whack-final-newline (string-tail string 2)))))
                 
                 (string->symbol
                  (nth-word 1 string)))))
         (if (and (string? data)
                  (zero? (string-length data)))
             (list keyword)
           (list keyword data))))

     (let ((line (make-data-from-string (car lines))))
       (set! lines (cdr lines))
       (append
        line
        (let loop ((gedcoms '()))
          (if (or
               (null? lines)
               (<= (line-level (car lines))
                   level))
              gedcoms
            (loop (cons (make-gedcom (+ 1 level))
                        gedcoms))))))))
  (display "Parsing file ... ") (force-output)
  (set! gdb (make-gedcom 0))
  (display "done") (newline))
