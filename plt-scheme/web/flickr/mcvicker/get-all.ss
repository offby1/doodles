(module get-all mzscheme
(require
  (planet "flickr.ss" ("dvanhorn" "flickr.plt" 1))
  (planet "sxml.ss" ("lizorkin" "sxml.plt"))
  (lib "match.ss")
  (lib "pretty.ss")
  (only (lib "1.ss" "srfi")
        second)
  (lib "trace.ss")
  "keys.ss")

;; get _all_ photos, not just the first page.  Provide some feedback
;; while we're at it, too, in case it takes a while.

(define (get-all-photos page-progress-proc . args)

  (define (get-page page-number)
    (when (zero? page-number)
      (error 'get-page "ah ah ah -- flickr page numbers count from 1, not 0"))

    (parameterize ((non-text-tags (list* 'photos (non-text-tags))))
      (let ((got (apply flickr.photos.search
                        (list* #:page (number->string page-number)
                               args))))
        (match got
               [(('photos
                  (('page page)
                   ('pages pages)
                   ('perpage perpage)
                   ('total total))
                  _ _ ...))
                (page-progress-proc page pages)]
               [(_ ...) #t])

        got)))
  (define (empty? page)
    (match page
           ;; this clause matches the case when we ask for page 22,
           ;; and there aren't that many pages: it returns something
           ;; that looks like

           ;; ((photos ((page "1") (pages "3") (perpage "2") (total
           ;; "6"))))

           ;; that is, it says it's page "1", but there isn't any
           ;; actual photo data in the returned stuff.

           [(('photos (_ _ _ ('total t))))
            #t]
           [(('photos
              (_ _ _ ('total t))
              _ ...))
            (equal? t "0")]))

  (define (extract-interesting-stuff page)
    (match page
           [(('photos metadata guts ...))
            guts]))

  (when (member #:page args)
    (error 'get-all-photos "Don't pass the #:page keyword"))

  (let loop ((pages-got 0)
             (results '()))
    (let ((one-page (get-page (add1 pages-got))))
      (if (empty? one-page)
          results
          (loop (add1 pages-got)
                (append
                 (extract-interesting-stuff one-page)
                 results))))))

(provide (all-defined))

;;; (pretty-print
;;;  (get-all-photos (lambda (this-page total-pages)
;;;                    (fprintf (current-error-port)
;;;                             "Processing page ~a of ~a~%"
;;;                             this-page total-pages)
;;;                    (flush-output (current-error-port)))
;;;                  #:user_id "10665268@N04"
;;;                  #:tags "chipping"))

)
