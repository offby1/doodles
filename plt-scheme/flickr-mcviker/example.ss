(module example mzscheme
(require (lib "mred.ss" "mred")
         (lib "trace.ss")
         (planet "flickr.ss" ("dvanhorn" "flickr.plt" 1))
         (lib "external.ss" "browser")
         (lib "url.ss" "net")
         (lib "match.ss")
         (lib "file.ss")
         (lib "class.ss")
         (lib "mred.ss" "mred"))

(current-api-key "d964b85147ddd4082dc029f371fe28a8")
(current-sec-key "4f5c414c39ee71a6")

;; Make a frame by instantiating the frame% class
(define frame (instantiate frame% ("Flickr Thingy")))

;; Make a static text message in the frame
(define msg (instantiate message% ("Hi." frame)))

(define (exn:flickr:invalid-auth-token? exn)
  (and (exn:flickr? exn)
       (= 98 (exn:flickr-code exn))))

(define (authenticate!)
  (parameterize ((sign-all? #t))
    (match (flickr.auth.getFrob)
           [(('frob () frob))
            (begin
              (send-url (url->string (authorize-url #:frob frob #:perms "read")))
              (message-box "OK!" "Do the web-browser thing" #f)
              (parameterize ((non-text-tags (list* 'auth (non-text-tags))))
                (match (flickr.auth.getToken #:frob frob)
                       [(('auth ()
                                ('token () token)
                                ('perms () perms)
                                ('user (('fullname fn) ('nsid nsid) ('username user)))))
                        (put-preferences (list 'flickr:token) (list token))])))])))

(trace authenticate!)
(define (maybe-authenticate!)
  (let ((auth-token (get-preference 'flickr:token)))
    (if auth-token
        (with-handlers ((exn:flickr:invalid-auth-token?
                         (lambda (exn) (authenticate!))))
          (parameterize ((sign-all? #t))
            (message-box
             "flickr.auth.checkToken sez:"
             (format "~s" (flickr.auth.checkToken #:auth_token auth-token))
             #f)
            (values)))
        (authenticate!))))

(trace maybe-authenticate!)
(define (run-example!)
  (maybe-authenticate!)
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
                     farm server id secret))])))

(trace run-example!)
(run-example!)
)
