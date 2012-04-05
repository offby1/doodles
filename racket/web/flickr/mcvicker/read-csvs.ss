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
         (lib "etc.ss")
         (only (lib "misc.ss" "swindle")
               regexp-case)
         (planet "csv.ss" ("neil" "csv.plt" 1 1)))

(define-struct datum (slide-number mount-date subject mount-notation scanned) #f)

(define (zero-pad i)
  (string-append
   (if (< i 10)
       "0"
       "")
   (number->string i)))

;; "Aug-02", e.g., => ("2002-08" "4")
;; "1968" => ("1968" "6")

;; the first value is just the same information as the input string,
;; but formatted the way flickr wants it.

;; the second value describes the accuracy of the first value.  "4"
;; means "sometime within the named month"; "6" means "sometime within
;; the named year".  See
;; http://www.flickr.com/services/api/misc.dates.html

;; "LEM" are our customer's initials.
(define (lem-date->flickr-date-info mount-date)

  (define month-sym->number
    (let ((month-symbols '(jan feb mar apr may jun jul aug sep oct nov dec)))
      (lambda (ms)
        (let ((sublist (member ms month-symbols)))
          (when (not sublist)
            (error 'make-datum "Can't parse date ~s -- \"~s\" isn't a proper month abbreviation"
                   mount-date ms))
          (add1
           (- (length month-symbols)
              (length sublist)))))))

  (define (normalize-year y)
    (let ((y (cond
              ((string? y)
               (string->number y))
              (else
               y))))
      (cond
       ((< y 30)
        (+ y 2000))
       ((< y 100)
        (+ y 1900))
       (else
        y))))

  (regexp-case
   (string-downcase mount-date)
   [#px"^[ ?]*$" (values #f #f)]
   [(#px"^([0-9]{2,4})$" year)
    (values
     (format "~a" (normalize-year year))
     "6")]
   [(#px"^([a-z]{3})-([0-9]{2,4})$" mon year)
    (values
     (format
      "~a-~a"
      (normalize-year year)
      (zero-pad (month-sym->number (string->symbol mon))))
     "4")]
   [else
    (error 'make-datum "Can't parse date ~s -- doesn't match the regexp" mount-date)]))

(define (private-make-datum slide-number mount-date subject mount-notation scanned)
  (make-datum
   slide-number
   (call-with-values
       (lambda ()
         (lem-date->flickr-date-info mount-date))
     cons)
   subject
   mount-notation
   scanned))

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
                       (apply private-make-datum row)))))))
             (else
              (status-proc
               (format
                "Freaky row has ~a entries, but I want 5 or 6: ~s~%"
                (length row)
                row))))))
        ip)))))

(provide snorgle-file *data-by-number*
         (struct datum (slide-number
                        mount-date
                        mount-notation
                        subject)))
)
