#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mred -M errortrace --no-init-file --mute-banner --version --require "$0"
|#
(module example mzscheme
(require (planet "flickr.ss" ("dvanhorn" "flickr.plt" 1))
         (lib "mred.ss" "mred")
         (lib "class.ss")
         (lib "match.ss")
         (lib "file.ss")
         (lib "external.ss" "browser")
         "auth.ss")

(maybe-authenticate!
 (lambda ()
   (message-box "OK!" "Do the web-browser thing" #f)))
(message-box "OK!" "Authenticated." #f)
(parameterize ((non-text-tags (list* 'photos (non-text-tags)))
               (sign-all? #t))
  (match (flickr.photos.search #:user_id "me" #:auth_token (get-preference 'flickr:token))
         [(('photos _ ('photo (('farm farm)
                               ('id id)
                               ('isfamily _)
                               ('isfriend _)
                               ('ispublic _)
                               ('owner owner)
                               ('secret secret)
                               ('server server)
                               ('title _))) . rest))
          (send-url
           (format "http://farm~a.static.flickr.com/~a/~a_~a_t.jpg"
                   farm server id secret))]))
)
