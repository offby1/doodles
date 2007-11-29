(module keys mzscheme
(require (planet "flickr.ss" ("dvanhorn" "flickr.plt" 1)))
(current-api-key "d964b85147ddd4082dc029f371fe28a8")
(current-sec-key "4f5c414c39ee71a6")

(define ed?
  (make-parameter
   (not (equal? (getenv "USERNAME") "Eric"))))

(define (*user-id*)
  (if (ed?)
      "10665268@N04"
      "20825469@N00"))

(define (*pref-name*)
  (if (ed?)
      'flickr:token:ed
      'flickr:token:me))

(provide ed?
         *pref-name*
         *user-id*)
)