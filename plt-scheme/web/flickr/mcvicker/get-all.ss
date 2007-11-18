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

(define (get-all-photos progress-proc . args)

  (define (get-page zero-based-number)
          (apply flickr.photos.search
                 (list* #:page (number->string (add1  zero-based-number))
                        #:per_page "2"
                        #:tag_mode "all"
                        args)))

  (define (contains-data? page)
    (match page
           [(('photos
              (_ _ _ ('total t))
              _ ...))
            (not (equal? t "0"))]))

  (define (extract-interesting-stuff page)
    page)

  (trace get-page)
  (trace contains-data?)
  (trace extract-interesting-stuff)

  (when (member #:page args)
    (error 'get-all-photos "Don't pass the #:page keyword"))

  (let loop ((pages-got 0)
             (results '()))
    (let ((one-page (get-page (add1 pages-got))))
      (if (contains-data? one-page)
          (loop (add1 pages-got)
                (cons (extract-interesting-stuff one-page)
                      results))
          results))))

(provide (all-defined))
(get-all-photos (lambda (a b c)
                  11) #:tags "ham,sandwich,bread")

)
