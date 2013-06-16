#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket $0
|#

#lang racket

(module+ test
  (require rackunit rackunit/text-ui))

(require (planet neil/csv:2:0)
         (only-in "misc.rkt" datum))

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

  (match (string-downcase mount-date)
    [(regexp #px"^([0-9]{2,4})$" (list _ year))
     (values
      (format "~a" (normalize-year year))
      "6")]
    [(regexp #px"^((?i:[a-z]){3})-([0-9]{2,4})$" (list _ mon year))
     (values
      (format
       "~a-~a"
       (normalize-year year)
       (zero-pad (month-sym->number (string->symbol mon))))
      "4")]
    [else
     (values #f #f)]))

(module+ test
  (check-equal? (call-with-values (thunk (lem-date->flickr-date-info "")) cons)
                (cons #f #f))
  (check-equal? (call-with-values (thunk (lem-date->flickr-date-info "1964")) cons)
                (cons "1964" "6"))
  (check-equal? (call-with-values (thunk (lem-date->flickr-date-info "Oct-64")) cons)
                (cons "1964-10" "4")))

(define (row-to-hash r)
  (for/hash ([k '(slide-number mount-date subject mount-notation scanned)]
             [v r])
    (values k v)))

(define (get h k) (hash-ref h k ""))

(define (hash-to-datum h)
  (datum
   (get h 'slide-number)
   (call-with-values
       (lambda ()
         (lem-date->flickr-date-info (get h 'mount-date)))
     cons)
   (get h 'subject)
   (get h 'mount-notation)
   (get h 'scanned)))

(define (snorgle-file fn [status-proc #f])
  (define *data-by-number* (make-hash '()))
  (when status-proc
    (status-proc (format
                  "~a ... " fn)))
  (call-with-input-file
      fn
    (lambda (ip)
      (csv-for-each
       (lambda (row)
         (let* ([h (row-to-hash row)]
                [slide-number (get h 'slide-number)])
           (when (integer? (read (open-input-string slide-number)))
             (hash-set!
              *data-by-number*
              slide-number
              (hash-to-datum h)))))
       ip)))
  *data-by-number*)

(provide snorgle-file)

(module+ main
  (for ([f  (directory-list ".")])
    (let ([m (regexp-match #px"\\.csv$" f)])
      (when m
        (pretty-write (snorgle-file f))))))
