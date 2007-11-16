#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module yow mzscheme
(require (only (lib "1.ss" "srfi")
               partition)
         (lib "pretty.ss")
         (lib "etc.ss")
         (planet "sxml.ss" ("lizorkin" "sxml.plt" 1)))

(define *doc*
  (sxml:document
   (path->string
    (build-path
     (this-expression-source-directory)
     "Robinson scans 1-200 for eric.xml"))
   '((o  . "urn:schemas-microsoft-com:office:office")
     (e  . "urn:schemas-microsoft-com:office:excel")
     (ss . "urn:schemas-microsoft-com:office:spreadsheet"))))

(define-struct datum (slide-number mount-date subject mount-notation scanned) #f)

(let-values (((complete-rows incomplete-rows)
              (partition
               (lambda (row)
                 (= 5 (length ((sxpath '(ss:Cell)) row))))
               (cdr ((sxpath '(ss:Workbook ss:Worksheet ss:Table ss:Row))
                     *doc*)))))
  (printf "An incomplete row:~%")
  (pretty-print (car incomplete-rows))
  (printf "A complete row:~%")
  (pretty-print (car complete-rows))

  (fprintf (current-error-port)
           "~a complete rows; ~a incomplete rows~%"
           (length complete-rows)
           (length incomplete-rows))

  ;; now look for rows that contain incomplete cells.
  (let-values (((short-cells ok-cells)
                (partition
                 (lambda (cell)
                   (null? ((sxpath '(ss:Data))
                           cell)))
                 ((sxpath '(ss:Cell))
                  complete-rows))))

    (when (not (null? short-cells))
      (printf "An incomplete cell: ~%")
      (pretty-print (car short-cells)))

    (when (not (null? ok-cells))
      (printf "A complete cell:~%")
      (pretty-print (car ok-cells)))))

)