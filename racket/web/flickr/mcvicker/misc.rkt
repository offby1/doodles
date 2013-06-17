#lang racket
; Hey Emacs, this is -*-scheme-*- code!

;; Run my tests with ``raco test racket-script-template.rkt''.
;; Invoke my "main" with ``racket racket-script-template.rkt''.

(require
 (planet dvanhorn/flickr:2:3)
 (planet "html-parser.ss" ("ashinn" "html-parser.plt" 1 1))
 (only-in "keys.rkt" *pref-name*))

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
  (define mapped
    (hash-map
     *photos-by-title*
     (lambda (title photo)
       (let ((as-number (title->number-or-false title)))
         (let ((datum (hash-ref *data-by-number* as-number #f)))
           (and
            datum
            (full-info
             title
             datum
             photo)))))))

  (filter
   (lambda (record)
     (match record
       [(struct full-info _)
        (let ([md (datum-mount-date (full-info-csv-record record))])
          (and (car md)
               (cdr md)))]
       [_ #f]))
   mapped))

(provide whop-record!)
(define (whop-record! record success-logger [for-real? #f])
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

      (when for-real?
        (flickr.photos.setDates
         #:auth_token (get-preference (*pref-name*))

         #:photo_id (photo-id (full-info-flickr-metadata record))
         #:date_taken date
         #:date_taken_granularity granularity))

      (if  (equal?  descr '(html "" "" ""))
           (log! (format "Skipping ~s because the description is empty" record))
           ;; I'm pretty sure
           ;; there's never any
           ;; return value, but
           ;; you can't be too careful!
           (when for-real?
             (let ((rv (flickr.photos.setMeta
                        #:auth_token (get-preference (*pref-name*))

                        #:photo_id  (photo-id (full-info-flickr-metadata record))
                        #:title (full-info-title record)
                        #:description (sxml->html descr))))
               (success-logger (format
                                "sent data to flickr: ~s: ~s: ~s => ~s"
                                (full-info-title record)
                                date
                                descr
                                rv))))
           ))))
