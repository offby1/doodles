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
         (only (lib "13.ss" "srfi") string-join )
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

;; TODO -- an empty QUERY-STRING, or one that consists entirely of
;; whitespace, causes us to get no results back.
(define (url-for-one-interesting-cat-photo query-string)
  (let* ((results (flickr.photos.search
                   'tags     (string-join (list "cat" query-string) ",")
                   'tag_mode "all"
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
