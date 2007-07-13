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
         (lib "pretty.ss")
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
                      "literate" "literary" "well-read" "bookish"
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
               (id       (car ((sxpath '(@ id *text*)) chosen-photo)))
               (photo-info (flickr.photos.getInfo
                            'photo_id id))
               (descr     ((sxpath '(photo description *text*)) photo-info))
               (urls      ((sxpath '(photo urls)) photo-info))
               (photopage-url (car ((sxpath '(// (url (@ (equal? (type "photopage")))) *text*)) urls)))
               (photo-info (flickr.photos.getSizes 'photo_id id))

               ;; don't ask for "Original"; it requires special
               ;; handling which I haven't yet done
               (size-info ((sxpath '(// (size (@ (equal? (label "Medium")))))) photo-info))
               (width    (car ((sxpath '(@ width       *text*)) size-info)))
               (height   (car ((sxpath '(@ height      *text*)) size-info)))
               (title    (car ((sxpath '(@ title       *text*)) chosen-photo)))
               (nsid     (car ((sxpath '(@ owner       *text*)) chosen-photo)))
               (person   (flickr.people.getInfo 'user_id nsid))
               (username (car ((sxpath '(person username *text*)) person)))
               ;; '(// (div (@ (equal? (class "g"))))))
               )

          `((tr
             (td (a ((href ,photopage-url))
                    (img ((src ,(url-for-photo chosen-photo 'medium))
                          (height ,height)
                          (width  ,width))))))

            (tf (td (p ,(if (null? descr)
                            "no description"
                          (list 'i (car descr))))))
            (tr (td (p ,(format
                         "That's ~a ~s cat (~s, from ~s).  Cute, huh?"
                         (article adjective)
                         adjective
                         title
                         username
                         ))))
            ))

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