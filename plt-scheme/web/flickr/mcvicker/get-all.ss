;;$Id$
(module get-all mzscheme
(require
 (planet "flickr.ss" ("dvanhorn" "flickr.plt" 1))
 (only (planet "memoize.ss" ("dherman" "memoize.plt" )) define/memo*)
 (lib "etc.ss")
 (lib "match.ss")
 (lib "pretty.ss")
 (lib "trace.ss"))

(define *cache* #f)
(define (alist->mutable-hash a)
  (let ((h (make-hash-table 'equal)))
    (for-each (lambda (p)
                (hash-table-put!
                 h
                 (car p)
                 (cdr p)))
              a)
    h))

(define-struct photo (id title))

;; returns a pair: the total number of pages (or 0 to indicate you
;; asked for a page past the end), and the list of photos from that
;; page.

;; This is defined at top level, rather than inside for-each-page,
;; because for some reason memoization doesn't seem to work when it's
;; defined inside for-each-page.
(define/memo* (get-one-page page-number . args)
  (when (zero? page-number)
    (error 'get-page "ah ah ah -- flickr page numbers count from 1, not 0"))

  (parameterize ((non-text-tags (list* 'photos (non-text-tags)))
                 (sign-all? #t))
    (match
     (hash-table-get
      *cache*
      page-number
      (lambda ()
        (let ((got (apply flickr.photos.search
                          (list* #:page (number->string page-number)
                                 #:sort "date_posted_asc"
                                 args))))
          (hash-table-put! *cache* page-number got)
          got)))
     [(('photos atts photos ...))
      (match atts
             [(('page this-page) ('pages pages) ('perpage _) ('total _))
              (cons (string->number pages)
                    (map (lambda (p)
                           (match p
                                  [('photo
                                    (('farm _)
                                     ('id id)
                                     ('isfamily _)
                                     ('isfriend _)
                                     ('ispublic _)
                                     ('owner _)
                                     ('secret _)
                                     ('server _)
                                     ('title title)))
                                   (make-photo id title)])) photos))])])))

;; for each page, calls proc on the list of photos from that page.
(define (for-each-page proc . args)
  (define *cache-file* "downloaded-photos-cache.ss")
  (set! *cache*
        (if (file-exists? *cache-file*)
            (alist->mutable-hash
             (with-input-from-file *cache-file* read))
             (make-hash-table 'equal)))
  (let loop ((pages-requested 0))
    (match-let ([(total-pages buncha-photos ...)
                 (apply get-one-page (add1 pages-requested) args)])
      (when (positive? total-pages)
        (proc
         buncha-photos
         (add1 pages-requested)
         total-pages)
        (loop (add1 pages-requested)))))

  (call-with-output-file
      *cache-file*
    (lambda (op)
      (parameterize ((print-hash-table #t))
        (pretty-print (hash-table-map *cache* cons) op)))

    'replace))

(provide (all-defined))

)
