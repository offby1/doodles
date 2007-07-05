#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; Do a simple web search via Amazon's "Alexa" search engine.

(module alexa mzscheme
(require (lib "url.ss" "net"))
(define *the-query* (make-url
                     "http"
                      #f ;; user
                      "wsearch.amazonaws.com" ;; host
                      #f ;; port
                      #t ;; path-absolute?
                      '();;  path
                      '(
                        (AWSAccessKeyId . "9876543212345123")
                        (Timestamp . "2007-01-26T01%3A15%3A38.000Z")
                        (Signature . "oQkiPZUtQ9PlTI2l4OTRA8fjYsM%3D")
                        (Version . "2007-03-15")
                        (Action . "Search")
                        (ResponseGroup . "Context")
                        (Query . "cats")
                        ) ;; query
                      #f ;;fragment
                      ))
)
