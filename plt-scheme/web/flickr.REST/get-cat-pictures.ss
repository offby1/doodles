#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qtmv "$0" --eval '(main)'
|#

;; Try out playing with the flickr API, via XML RPC.  (The API is
;; available in lots of other flavors too, like REST and SOAP, but
;; only XML RPC has a handy PLaneT package.)

(module get-cat-pictures mzscheme
(require (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
         (lib "trace.ss")
         (lib "sendurl.ss" "net")
         (lib "file.ss")
         (only (lib "13.ss" "srfi") string-join )
         (only (lib "os.ss") gethostname)
         (only (lib "url.ss" "net")
               get-pure-port
               string->url
               )
         "flickr.ss")
(provide
 all-interesting-cat-photos
 attribute-getter-from-sxml
 main
 url-for-photo
 )

;; It's hard to explain what this does, other than save typing.  Just
;; see how I use it, and it should become obvious
(define (attribute-getter-from-sxml sxml path)
  (lambda (attname)
    (car ((sxpath `(,@path @ ,attname *text*)) sxml))))

(define (all-interesting-cat-photos query-string)
  (flickr.photos.search
   'tags     (string-join (list "cat" query-string) ",")
   'tag_mode "all"
   'sort     "date-taken-asc"

   ;; minimum_longitude, minimum_latitude, maximum_longitude,
   ;; maximum_latitude.

   ;; Pacific Northwest, more or less
   ;; 'bbox     "-124,46,-121,49"
   ))

;; TODO -- an empty QUERY-STRING, or one that consists entirely of
;; whitespace, causes us to get no results back.

(define (url-for-photo photo size)
  (let* ((@ (attribute-getter-from-sxml photo '())))

    ;; believe it or not, kludging up a URL out of pieces like this
    ;; is officially sanctioned.  See
    ;; http://www.flickr.com/services/api/misc.urls.html

    ;; note that photos can come in different sizes; this URL is for
    ;; the "medium" size.

    (format
     "http://farm~a.static.flickr.com/~a/~a_~a~a.jpg"
     (@ 'farm)
     (@ 'server)
     (@ 'id)
     (@ 'secret)
     (case size
       ((smallsquare) "_s")
       ((thumbnail)   "_t")
       ((small)       "_m")
       ((medium)      "" )
       ((large)       "_b")
       (else "t")
       ))))

;; a little test
(define (main)
  (let ((cats  (all-interesting-cat-photos "white")))
    (printf "~a~%" ((sxpath '(photos @ total)) cats))))
)
