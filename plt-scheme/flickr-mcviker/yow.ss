#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module yow mzscheme
(require (only (lib "1.ss" "srfi")
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
              (let ((index  (read (open-input-string (car row)))))
                (when (integer? index)
                  (hash-table-put!
                   *data-by-number*
                   index
                   (apply make-datum row)))))
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
  "j8001-j9000.csv")
 )

;;; (for-each (lambda (pair)
;;;             (printf "~s: ~s~%"
;;;                     (car pair)
;;;                     (cdr pair)))
;;;           (hash-table-map *data-by-number* cons))

;; just for fun, print all the distinct dates that appear
(let ((dates (make-hash-table 'equal)))
  (hash-table-for-each
   *data-by-number*
   (lambda (k v)
     (hash-table-put!
      dates
      (datum-mount-date v)
      (add1 (hash-table-get dates (datum-mount-date v) 0)))))
  (pretty-print (hash-table-map dates cons)))
)