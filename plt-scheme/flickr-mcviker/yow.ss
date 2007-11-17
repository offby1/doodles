#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module yow mzscheme
(require (only (lib "1.ss" "srfi")
               filter
               partition
               take)
         (lib "pretty.ss")
         (lib "etc.ss")
         (planet "csv.ss" ("neil" "csv.plt" 1 1)))

(file-stream-buffer-mode (current-error-port) 'line)
(file-stream-buffer-mode (current-output-port) 'line)

(define-struct datum (slide-number mount-date subject mount-notation scanned) #f)

(define *data-by-number* (make-hash-table 'equal))

(for-each
 (lambda (fn)
   (fprintf (current-error-port)
            "~a ... " fn)
   (call-with-input-file
       (build-path
        (this-expression-source-directory)
        fn)
     (lambda (ip)
       (csv-for-each
        (lambda (row)
          (let again ((row row))
            (cond
             ((and (= 6 (length row))
                   (zero? (string-length (list-ref row 5))))
              (again (take row 5)))
             ((= 5 (length row))
              (let ((non-empty-fields (filter (lambda (str)
                                                (positive? (string-length str)))
                                              row)))
                ;; don't bother storing the data if the index is the
                ;; only non-empty field.
                (when (< 1 (length non-empty-fields))
                  (let ((index  (read (open-input-string (car row)))))
                    (when (integer? index)
                      (hash-table-put!
                       *data-by-number*
                       index
                       (cons
                        (apply make-datum row)
                        (hash-table-get *data-by-number* index '()))))))))
             (else
              (fprintf (current-error-port)
                       "Freaky row: ~s~%" row)))))
        ip)))
   (newline (current-error-port)))
 (list
  "j1-j1000.csv"
  "j1000-j2000.csv"
  "j2000-j3000.csv"
  "j3000-j4000.csv"
  "j4000-j5000.csv"
  "j5000-j6000.csv"
  "j6000-j7000.csv"
  "j7001-j8000.csv"
  "j8001-j9000.csv"))

(pretty-print
 (take
  (map cdr (hash-table-map *data-by-number* cons))
  3))
)