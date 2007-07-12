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
         (lib "servlet.ss" "web-server")
         (file "/home/erich/doodles/plt-scheme/web/flickr/get-cat-pictures.ss"))

(provide interface-version timeout start)

(define interface-version 'v1)
(define timeout +inf.0)

(define *adjectives* (list
                      "annoyed"
                      "electric"
                      "fierce"
                      "insane"
                      "lethargic"
                      "loyal"
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
  (let ((choice (list-ref *adjectives* (random (length *adjectives*)))))

    `((tr
       (td (img ((src ,(url-for-one-interesting-cat-photo choice))))))
      (tr (td (p ,(format
                   "That's ~a ~s cat.  Cute, huh?"
                   (article choice)
                   choice)))))))

(define (start initial-request)

  (with-errors-to-browser
   send/finish
   (lambda ()
     `(html (body (table
                   ,@(random-rows)
                   )))))
  )
)