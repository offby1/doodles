#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module read-csvs mzscheme
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

(define snorgle-file
 (lambda (fn status-proc)
   (status-proc (format
                 "~a ... " fn))
   (call-with-input-file
       fn
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
              (status-proc
               (format
                "Freaky row: ~s~%" row))))))
        ip)))))

(provide
 snorgle-file
 *data-by-number*)
)