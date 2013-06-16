#lang racket
; Hey Emacs, this is -*-scheme-*- code!

;; Run my tests with ``raco test racket-script-template.rkt''.
;; Invoke my "main" with ``racket racket-script-template.rkt''.

(module+ test
  (require rackunit rackunit/text-ui))

(provide log!)
(define log! (make-parameter #f))

(provide title->number-or-false)
(define (title->number-or-false string)
  (match string
    [(regexp #px"^[a-zA-Z]([0-9]+)\\b" (list _ number-string))
     (string->number number-string)]
    [_ #f]))

(module+ test
  (check-equal? (title->number-or-false "j123") 123)
  (check-equal? (title->number-or-false "123") #f)
  (check-equal? (title->number-or-false "jlkmn") #f)
  (check-equal? (title->number-or-false "J6100") 6100)
  (check-equal? (title->number-or-false "X6100") 6100)
  )

(provide (struct-out full-info))
(struct full-info (title csv-record flickr-metadata) #:transparent)

(provide (struct-out datum))
(struct datum (slide-number mount-date subject mount-notation scanned) #:transparent)

(provide (struct-out photo))
(struct photo (id title) #:transparent)

(provide join)
(define (join *photos-by-title* *data-by-number*)
  (filter
   (lambda (record)
     (match record
       [(struct datum _)
        (let ([md (datum-mount-date (full-info-csv-record record))])
          (and (car md)
               (cdr md)))]
       [_ #f]))
   (hash-map
    *photos-by-title*
    (lambda (title photo)
      (let ((as-number (title->number-or-false title)))
        (and (integer? as-number)
             (let ((datum (hash-ref *data-by-number* as-number #f)))
               (and
                datum
                (full-info
                 title
                 datum
                 photo)))))))))

(provide whop-record!)
(define (whop-record! record success-logger)
  (match-let ([(cons date granularity)
               (datum-mount-date (full-info-csv-record record))])
    (let* ((mn (datum-mount-notation  (full-info-csv-record record)))
           (s  (datum-subject         (full-info-csv-record record)))
           (descr
            `(html
              ,(if (positive? (string-length mn)) (list 'b mn) "")
              ,(if (positive? (string-length mn))
                   "|| " "")
              ,(if (positive? (string-length s))  s       "")

              )))

      (if  (equal?  descr '(html "" "" ""))
           (log! (format "Skipping ~s because the description is empty" record))
           ;; I'm pretty sure
           ;; there's never any
           ;; return value, but
           ;; you can't be too careful!
           (success-logger (format
                            "Pretending to send data: ~s: ~s: ~s"
                            (full-info-title record)
                            date
                            descr))))))
