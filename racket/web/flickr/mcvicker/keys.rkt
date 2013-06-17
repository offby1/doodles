#lang racket

(require (planet dvanhorn/flickr:2:3))
(current-api-key "d964b85147ddd4082dc029f371fe28a8")
(current-sec-key "4f5c414c39ee71a6")

(define ed?
  (make-parameter
   (not
    (regexp-match
     #rx"(?i:^eric)"
     (or (getenv "USERNAME")
         (getenv "USER"))))))

(define (*user-id*)
  (if (ed?)
      "76532524@N03"
      "20825469@N00"))

(define (*pref-name*)
  (string->symbol (format "flickr:token:~a" (*user-id*))))

(provide ed?
         *pref-name*
         *user-id*)
