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
  (when (member #:page args)
    (error 'get-all-photos "Don't pass the #:page keyword"))
  (let* ((first-page (apply flickr.photos.search (list* #:page "1" #:per_page "2" args)))
         (metadata (second (car first-page))))
    (pretty-print metadata)
    (match metadata
           [(('page    page)
             ('pages   pages)
             ('perpage perpage)
             ('total   total))
            (let loop ((this-page first-page)
                       (page (string->number page))
                       (pages-to-process (sub1 (string->number pages)))
                       (photos ((sxpath '(photo)) first-page)))
              (printf "page ~a of ~a; ~a per page; ~a total~%"
                      page pages perpage total)
              (if (zero? pages-to-process)
                  (display (map photo-sxml->url photos))
                  (loop (apply flickr.photos.search (list* #:page (number->string page)
                                                           #:per_page "2"
                                                           args))
                        (add1 page)
                        (sub1 pages-to-process)
                        (cons ((sxpath '(photo)) this-page) photos))))])))

(define (photo-sxml->url p)
  (match p
         [('photo (('farm farm)
                   ('id id)
                   ('isfamily _)
                   ('isfriend _)
                   ('ispublic _)
                   ('owner owner)
                   ('secret secret)
                   ('server server)
                   ('title _)))
          (format "http://farm~a.static.flickr.com/~a/~a_~a_t.jpg"
                  farm server id secret)]))

(provide (all-defined))
(get-all-photos (lambda (a b c)
                  11) #:tags "fred")

)
