#!/usr/bin/guile -s
!#

;; Find all the acronyms in some files, and display them, one per
;; line.

;; An acronym is simply a sequence of upper-case letters, such as
;; `AFAIK' ("as far as I know").

;; for each line
;;   for each match for "\\b[A-Z]+\\b"
;;     add the match data to a hash

;; for each entry in the hash table
;;   display the entry on a line by itself

(use-modules (ice-9 debug))
(use-modules (ice-9 slib))
(use-modules (ice-9 regex))
(use-modules (ice-9 popen))

(require 'filter)                       ; for flatmap
(require 'sort)

(load "/home/offby1/doodles/guile/uniqify.scm")

(define (acronyms-in-file fn)

  (define (file->lines fn)

    (define (lines-from port)
      (let loop ((one-line (read-line port))
                 (result '()))
        (if (eof-object? one-line)
             result
          (loop (read-line port)
                (cons one-line result)))))
    (trace lines-from)
    (lines-from  
     (open-input-file fn)
     ;;(if (string-match "\\.gz$" fn) (open-input-pipe (string-append "zcat " fn)))
     ))
  (trace file->lines )
  (let ()
  
    (define acronyms-in-line
      (let ((r (make-regexp "\\b[A-Z][A-Z]+\\b")))
        (lambda (str)
          (let loop ((str str)
                     (results '()))
            (let ((matches (regexp-exec r str)))
              (if (not matches)
                  results
                (let ((acronym (match:substring matches)))
                  (loop (make-shared-substring str (match:end matches))
                        (cons acronym
                              results)))))))))
    (trace acronyms-in-line)
    (flatmap acronyms-in-line (file->lines fn))))

(trace acronyms-in-file)

(for-each
 (lambda (str)
   (display str)
   (newline))
 (sort 
  (uniqify 
   (flatmap 
    (lambda (fn)
      (display fn (current-error-port))
      (display "..." (current-error-port))
      (force-output  (current-error-port))
      (let ((return 
             (acronyms-in-file fn)
             ))
        (newline  (current-error-port))
        return))
    (cdr (command-line))))
  string<?))
