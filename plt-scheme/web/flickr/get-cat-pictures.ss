#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; Try out playing with the flickr API, via XML RPC.  (The API is
;; available in lots of other flavors too, like REST and SOAP, but
;; only XML RPC has a handy PLaneT package.)

(module get-cat-pictures mzscheme
(require (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
         (lib "pretty.ss")
         (lib "trace.ss")
         (lib "sendurl.ss" "net")
         (lib "file.ss")
         (only (lib "os.ss") gethostname)
         (only (lib "url.ss" "net")
               get-pure-port
               string->url
               )
         "flickr.ss")
(provide url-for-one-interesting-cat-photo)

;; It's hard to explain what this does, other than save typing.  Just
;; see how I use it, and it should become obvious
(define (attribute-getter-from-sxml sxml path)
  (lambda (attname)
    (car ((sxpath `(,@path @ ,attname *text*)) sxml))))

(define (url-for-one-interesting-cat-photo)
  (let* ((results (flickr.photos.search
                   'tags     "cat"
                   'tag_mode "all"
                   'sort     "interestingness-desc"
                   'bbox     "-122,47,-121,48" ;includes my house
                   ))
         (@ (attribute-getter-from-sxml results '(photos (photo 1)))))

    ;; believe it or not, kludging up a URL out of pieces like this is
    ;; officially sanctioned.

    (format
     "http://farm~a.static.flickr.com/~a/~a_~a.jpg"
     (@ 'farm)
     (@ 'server)
     (@ 'id)
     (@ 'secret))))

)
