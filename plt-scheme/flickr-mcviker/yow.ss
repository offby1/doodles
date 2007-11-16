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
         (planet "csv.ss" ("neil" "csv.plt" 1 1)))

(define-struct datum (slide-number mount-date subject mount-notation scanned) #f)

(define *data-by-number* (make-hash-table 'equal))

(let ((ip
       (open-input-file
        (build-path
         (this-expression-source-directory)
         "Robinson scans 1-200 for eric.csv"))))
  (csv-for-each
   (lambda (row)
     (let ((index  (read (open-input-string (car row)))))
       (when (integer? index)
         (hash-table-put!
          *data-by-number*
          index
          (apply make-datum row)))))
   ip)
  (close-input-port ip))

(for-each (lambda (pair)
            (printf "~s: ~s~%"
                    (car pair)
                    (cdr pair)))
          (hash-table-map *data-by-number* cons))

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