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

(define cat-photos-sxml
  (flickr.photos.search
   'tags     "snowball,cat"
   'tag_mode "all"
   'sort     "interestingness-desc"
   ))

;; It's hard to explain what this does, other than save typing.  Just
;; see how I use it, and it should become obvious
(define (attribute-getter-from-sxml sxml path)
  (lambda (attname)
    (car ((sxpath `(,@path @ ,attname *text*)) sxml))))

(define @ (attribute-getter-from-sxml cat-photos-sxml '(photos)))
(define *num-photos-returned* (string->number (@ 'total)))

;; This will soon (as of 5 July 2007) be available in mzscheme.
(define (port->bytes ip)
  (let loop ((result (make-bytes 0)))
    (let ((chunk (read-bytes 10000 ip)))
      (if (eof-object? chunk)
          result
        (begin (fprintf (current-error-port)
                        "~a bytes ... " (+ (bytes-length chunk)
                                           (bytes-length result)))
               (loop (bytes-append result chunk)))))))

(if (zero? *num-photos-returned*)
    (printf "Uh oh, no photos returned: ~a~%" cat-photos-sxml)
  (let ((@ (attribute-getter-from-sxml cat-photos-sxml '(photos (photo 1)))))

    (define photo-id (@ 'id))

    (define first-photo-info (flickr.photos.getInfo
                              'photo_id   photo-id))

    (let ((@ (attribute-getter-from-sxml first-photo-info '(photo)))

          ;; believe it or not, kludging up a URL out of pieces like
          ;; this is officially sanctioned.
          (bare-image-url
           (format "http://farm~a.static.flickr.com/~a/~a_~a.jpg" (@ 'farm) (@ 'server) photo-id (@ 'secret))))

      (printf "URL for the unadorned image: ~s~%" bare-image-url)

      (let ((jpeg-data
             (port->bytes (get-pure-port (string->url bare-image-url))))
            (tfn (make-temporary-file)))

        (call-with-output-file*
         tfn
         (lambda (op) (write-bytes jpeg-data op))
         'truncate)
        (printf "Wrote ~a bytes to ~a~%"
                (bytes-length jpeg-data)
                tfn)))))
)
