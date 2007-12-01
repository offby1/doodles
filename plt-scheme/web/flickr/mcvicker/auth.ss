#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#
(module auth mzscheme
(require (planet "flickr.ss" ("dvanhorn" "flickr.plt" 1))
         (lib "external.ss" "browser")
         (lib "url.ss" "net")
         (lib "match.ss")
         (lib "file.ss")
         "keys.ss")

(define (exn:flickr:invalid-auth-token? exn)
  (and (exn:flickr? exn)
       (= 98 (exn:flickr-code exn))))

(define (authenticate! browser-prompt-thunk)
  (parameterize ((sign-all? #t))
    (match (flickr.auth.getFrob)
           [(('frob () frob))
            (begin
              (send-url (url->string (authorize-url #:frob frob #:perms "write")))
              (browser-prompt-thunk)
              (parameterize ((non-text-tags (list* 'auth (non-text-tags))))
                (match (flickr.auth.getToken #:frob frob)
                       [(('auth ()
                                ('token () token)
                                ('perms () perms)
                                ('user (('fullname fn) ('nsid nsid) ('username user)))))
                        (put-preferences (list (*pref-name*)) (list token))])))])))

(define (maybe-authenticate! browser-prompt-thunk)
  (let ((auth-token (get-preference (*pref-name*))))
    (if auth-token
        (with-handlers ((exn:flickr:invalid-auth-token?
                         (lambda (exn) (authenticate! browser-prompt-thunk))))
          (values))
        (authenticate! browser-prompt-thunk))))

(provide maybe-authenticate!)

)
