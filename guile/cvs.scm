#!/usr/bin/guile -s
!#

; This answers a question I occasionally have: which files in a
; directory are under control of CVS (as opposed to being, say, built
; objects, or backups that Emacs created when I edited a file)?
;
; You pass it the name of a directory that has a CVS subdirectory, and
; it roots around in the "Entries" file and prints out the names of
; the files under CVS control.  A typical way to use it is this:
;
; find . -type d -name CVS -exec cvs.scm {}/.. \;

; alternatively:

; for i in $(find . -type d -name CVS ); do cvs.scm $(cd $i/..;pwd); done

(use-modules (ice-9 slib))
(require 'filter)
(require 'split)

(define (files-under-CVS-control directory-name)

  ;; Given a line from a CVS `Entries' file, return the name of the
  ;; file to which the line refers.  If the line refers to a
  ;; directory, return an empty string.

  (define (parse-entry-line str)
    (if (and (< 0 (string-length str))
             (char=? (string-ref str 0)
                     #\D))
        ;; ignore directories
        ""
      (car (split str "/"))))

  ;; Hack off any trailing slash
  (let* ((directory-name-length (string-length directory-name))
         (last-char (string-ref directory-name (- directory-name-length 1))))
    (if (char=? #\/ last-char)
        (set! directory-name (make-shared-substring directory-name 0
                                                    (-
                                                     directory-name-length 1)))))

  (let ((entries-file-name (string-append directory-name "/CVS/Entries")))
    (if (not (access? entries-file-name R_OK))
        '()
      (let* ((entries-port (open-input-file entries-file-name))
             (entry-lines
              (let loop ((one-line (read-line entries-port))
                         (results '()))
                (if (eof-object? one-line )
                    results
                  (loop (read-line entries-port)
                        (cons one-line results))))))
        (map
         (lambda (str)
           (string-append directory-name "/" str))
         (filter 
          (lambda (str)
            (< 0 (string-length str)))
          (map parse-entry-line entry-lines)))))))

(for-each 
 (lambda (str)
   (display str)
   (newline)) 
 (flatmap
  files-under-CVS-control
  (cdr (command-line))))
