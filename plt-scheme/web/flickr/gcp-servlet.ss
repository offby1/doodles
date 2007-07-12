#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module gcp-servlet mzscheme
(require (only  (lib "url.ss" "net")
                string->url
                url->string
                set-url-query!)
         (only (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
               sxpath)
         (lib "servlet.ss" "web-server")
         (file "/home/erich/doodles/plt-scheme/web/flickr/get-cat-pictures.ss")
         (file "/home/erich/doodles/plt-scheme/web/flickr/flickr.ss"))

(provide interface-version timeout start)

(define interface-version 'v1)
(define timeout +inf.0)

(define *adjectives* (list
                      "angry"
                      "annoyed"
                      "cold"
                      "electric"
                      "fierce"
                      "frozen"
                      "hungry"
                      "insane"
                      "lethargic"
                      "loyal"
                      "mad"
                      "old"
                      "scary"
                      "sexy"
                      "slow"
                      "stupid"
                      "ugly"
                      ))

(define (article word)
  (let ((first-letter (char-downcase (string-ref word 0))))
    (if (member first-letter '(#\a #\e #\i #\o #\u))
        "an"
      "a")))

(define (random-rows)
  (let* ((adjective (list-ref *adjectives* (random (length *adjectives*))))
         (pix (all-interesting-cat-photos adjective))
         (howmany (min (string->number (car ((sxpath '(photos @ total   *text*)) pix)))
                       (string->number (car ((sxpath '(photos @ perpage *text*)) pix))))))


    (if (positive? howmany)
        (let* ((chosen-photo ((sxpath `(photos (photo ,(add1 (random (sub1 howmany)))))) pix))
               (id ((sxpath '(@ id *text*)) chosen-photo))
               (sizes (flickr.photos.getSizes 'photo_id (car id)))
               (medium ((sxpath '(// (size (@ (equal? (label "Medium")))))) sizes))
               (width  (car ((sxpath '(@ width  *text*)) medium)))
               (height (car ((sxpath '(@ height *text*)) medium)))
               ;; '(// (div (@ (equal? (class "g"))))))
               )
          `((tr
             (td (img ((src ,(url-for-photo chosen-photo))
                       (height ,height)
                       (width  ,width)))))
            (tr (td (p ,(format
                         "That's ~a ~s cat ~a.  Cute, huh?"
                         (article adjective)
                         adjective
                         ((sxpath '(@ title *text*)) chosen-photo)
                         ))))))
      `((tr (td (p ,(format "Uh oh, I couldn't find any pictures for ~s" adjective))))))))

(define (start initial-request)

  (with-errors-to-browser
   send/finish
   (lambda ()
     `(html (body (table
                   ,@(random-rows)
                   )))))
  )
)