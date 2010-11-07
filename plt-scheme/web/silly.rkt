#lang web-server/insta

(require "kelly.rkt")

(define (start request)
  `(html
    (head (title "This House Is A Mess"))
    (body
     (p ,(format "I can't believe it's only been ~a days since Kelly was here." (days-since-kelly))))))

