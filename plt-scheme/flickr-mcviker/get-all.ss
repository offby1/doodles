(module get-all mzscheme
(require
  (planet "flickr.ss" ("dvanhorn" "flickr.plt" 1))
  (planet "sxml.ss" ("lizorkin" "sxml.plt"))
  (lib "match.ss")
  (lib "pretty.ss")
  (lib "trace.ss")
  "keys.ss")

;; get _all_ photos, not just the first page.  Provide some feedback
;; while we're at it, too, in case it takes a while.

(define (get-all-photos . args)
  (when (member #:page args)
    (error 'get-all-photos "Don't pass the #:page keyword"))
  (let ((first-page (apply flickr.photos.search (list* #:page "1" #:per_page "2" args))))

    (let ((photos ((sxpath '(photo)) first-page)))

      (display (map photo-sxml->url photos)))))

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
(get-all-photos #:tags "fred")

)
