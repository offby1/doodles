#lang scheme
(require web-server/servlet
         web-server/servlet-env
         web-server/insta/insta
         (only-in net/url set-url-query!)
         (prefix-in json: (planet dherman/json:1:1/json))
         "ssl-url.ss")

(define *our-url* "http://localhost:8000/servlets/standalone.ss")
(define (my-app request)
  (let ((bindings (request-bindings request)))
    (if (exists-binding?  'token bindings)
        (do-the-token-thing (extract-binding/single 'token bindings))
        `(html (head (title "Hello world!"))
               (body (p "Hey out there!")
                     (p
                      (a ((class "rpxnow")
                          (onclick "return false;")
                          (href
                           ,(let ((u (string->url "https://offby1-fooling-around.rpxnow.com/openid/v2/signin")))
                              (set-url-query! u `((token_url . ,*our-url*)))
                              (url->string u))))
                         "Sign in, why don'tcha")))

               (script ((src "https://rpxnow.com/openid/v2/widget")
                        (type "text/javascript"))
                       "// force a proper closing!")
               (script ((type "text/javascript"))
                       ,(string-append
                         "RPXNOW.token_url = \""
                         *our-url*
                         "\" ;"
                         "RPXNOW.realm = \"offby1-fooling-around\" ;"
                         "RPXNOW.overlay = true                  ;")

                       )
               ))))

(define (do-the-token-thing token)
  (call/input-url
   (string->url "https://rpxnow.com/api/v2/auth_info")
   (lambda (url)
     (set-url-query! url `((apiKey . "7ef12964d7aae382bf38110ac6b5deba680c569d")
                           (token . ,token)))
     (ssl:post-pure-port url #"")
     )
   (lambda (ip)
     (let ((op (open-output-string)))
       (fprintf op "Some JSON: ~a~%" (json:read ip))
       `(html
         (p ,(get-output-string op)))))))

(serve/servlet my-app)
