#lang web-server

(require
 web-server/dispatch
 web-server/formlets
 web-server/formlets/servlet
 web-server/page
 web-server/servlet-env
 )

(provide interface-version)
(define interface-version 'stateless)

(define-values (blog-dispatch blog-url)
  (dispatch-rules
   [("url" (string-arg)) expand-and-redirect-url]
   [else create-short-url-page]))

(define url-formlet
  (formlet
   (div
    "URL:" ,{input-string . => . url})
   (list url)))

(define/page (create-short-url-page)
  (match (send/formlet url-formlet)
    [(list (? string? u))
     (response/xexpr `(p ,(format "It was ~a" (url->string (string->url u)))))]))

(define (expand-and-redirect-url req)
  (response/xexpr
   `(html (p "Not implemented!"))))

(define (start req)
  (blog-dispatch req))

(serve/servlet start)
