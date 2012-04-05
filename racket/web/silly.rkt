#lang racket

(require web-server/servlet
         web-server/compat/0/coerce)
(require "kelly.rkt")

(provide/contract (start (request? . -> . response?)))
(define (start request)
  (let ([head "This House Is A Mess"])
    `(html
      (head (title ,head))
      (body
       (h1 ,head)
       (p ,(format "I can't believe it's only been ~a days since Kelly was here." (days-since-kelly)))))))

(require web-server/servlet-env)
(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 80
               #:servlet-path
               "/servlets/silly.rkt")
