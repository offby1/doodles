#lang racket

(require (planet dvanhorn/flickr:2:3)
         (lib "external.ss" "browser")
         (lib "url.ss" "net")
         (lib "match.ss")
         (lib "file.ss")
         "keys.rkt")

(define (exn:flickr:invalid-auth-token? exn)
  (and (exn:flickr? exn)
       (= 98 (exn:flickr-code exn))))

(define (authenticate! browser-prompt-thunk)
  (parameterize ((signed? #t))
    (match (flickr.auth.getFrob)
           [(('frob () frob))
            (begin
              (send-url (url->string (authorize-url (current-sec-key)
                                                    (cons 'api_key (current-api-key))
                                                    (cons 'frob frob)
                                                    (cons 'perms "write"))))
              (browser-prompt-thunk)
              (match (flickr.auth.getToken #:frob frob)
                [(('auth ()
                         ('token () token)
                         ('perms () perms)
                         ('user (('fullname fn) ('nsid nsid) ('username user)))))
                 (put-preferences (list (*pref-name*)) (list token))]))])))

(define (maybe-authenticate! browser-prompt-thunk)
  (let ((auth-token (get-preference (*pref-name*))))
    (if auth-token
        (with-handlers ((exn:flickr:invalid-auth-token?
                         (lambda (exn) (authenticate! browser-prompt-thunk))))
          (values))
        (authenticate! browser-prompt-thunk))))

(provide maybe-authenticate!)
