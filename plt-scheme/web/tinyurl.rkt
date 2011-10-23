#lang racket

(require
 (only-in "db.rkt" db-lookup db-add! db-init!)
 (only-in file/md5 md5)
 (only-in net/base64 base64-encode-stream)
 net/url
 web-server/dispatch
 web-server/formlets
 web-server/formlets/servlet
 web-server/http
 web-server/http/xexpr
 web-server/page
 )
;; Test by doing something like this in the REPL:
;; (response->list (dispatch (urlstring->request "wassup")))

(define-values (dispatch url-generator)
  (dispatch-rules
   [("") fancy-form-input]
   [("shorten" (string-arg))  create-short-url-page]
   [((string-arg)) expand-and-redirect]
   [else 404-page]))

(define (expand-and-redirect req shorty)
  (let ([found (db-lookup shorty)])
    (if found
        (redirect-to found)
        (404-page req))))

(define (get-host-and-port r)
  (let ([host-port-bytes (header-value (headers-assq* #"Host" (request-headers/raw r)))])
    (match (bytes->string/utf-8 host-port-bytes)
      [(regexp "(.*):(.*)" (list _ h port-string))
       (values h (string->number port-string))])))

(define (create-short-url-page req str)
  (let-values ([(hostname hostport) (get-host-and-port req)])
    (let ([urlstr
           (url->string
            (struct-copy
             url
             (request-uri req)
             [scheme "http"]
             [host (or (url-host (request-uri req))
                       hostname)]
             [port (or (url-port (request-uri req))
                       hostport)]
             [path-absolute? #t]
             [path (url-path
                    (string->url (url-generator expand-and-redirect (shorten-url-string str))))])
            )])
      (response/xexpr
       `(html (p "Your short URL is: "
                 (a ((href ,urlstr))
                    (tt ,urlstr))))))))

(define (fancy-form-input req)
  (let ([url-to-shorten
         (send/formlet
          (formlet
           (div "URL to shorten:" ,(input-string . => . url))
           url))])
    (redirect-to
     (url-generator create-short-url-page url-to-shorten))))

(define (404-page req)
  (response/xexpr
   `(html
     (p "Sorry, man; I just don't grok the URL")
     (p (tt ,(url->string (request-uri req)))))
   #:code 404
   #:message #"Dave's not here"))

(define (hash-it str)
  (bytes->string/utf-8
   (base64-encode
    (subbytes
     (md5 (string->bytes/utf-8 str) #f)
     0 6))))

;; just like the one in the library, except it doesn't append a
;; carriage-return/newline.
(define (base64-encode bytes)
  (let ((sop (open-output-bytes)))
    (base64-encode-stream (open-input-bytes bytes) sop "")
    (get-output-bytes sop)))

(define (shorten-url-string long)
  (let ([short (hash-it long)])
    ;; TODO -- combine the lookup with the add somehow?  Saves a
    ;; round-trip to the database.
    (let ([existing (db-lookup short)])
      (when (not existing)
        (db-add! short long))
      short)))


;; For testing
(define (urlstring->request u)
  (make-request
   #"GET"                               ;method
   (string->url u)                      ;URI
   empty                                ;headers/raw
   (delay empty)                        ;bindings/raw-promise
   #f                                   ;post-data/raw
   "1.2.3.4"                            ;host-ip
   80                                   ;host-port
   "4.3.2.1"                            ;client-ip
   ))

(define (response->list resp)
  `(
    (code ,(response-code resp))
    (message ,(response-message resp))
    (headers ,(map (lambda (h) (cons (header-field h)
                                     (header-value h))) (response-headers resp)))
    (body ,(let ([op (open-output-string)])
             ((response-output resp) op)
             (get-output-string op)))))

(define (dump r)
  (pretty-print
   `(
     (method ,(request-method r))
     (uri    ,(url->string (request-uri r)))
     (headers ,(map (lambda (h) (format "~a:~a" (header-field h) (header-value h))) (request-headers/raw r)))
     (bindings ,(map binding-id (request-bindings/raw r)))
     (host-ip ,(request-host-ip r))
     (host-port ,(request-host-port r))
     (client-ip ,(request-client-ip r))
     )
   ))

(provide start)
(define (start req)
  (dispatch req))

(provide go)
(define (go)
  (db-init!)
  (serve/dispatch dispatch))
