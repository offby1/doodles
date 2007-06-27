#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module xmlrpc mzscheme
(require (planet "xmlrpc.ss" ("schematics" "xmlrpc.plt" )))

(define betty (xmlrpc-server "betty.userland.com" 80 "RPC2"))
(define get-state-name (betty "examples.getStateName"))
(printf "State 42 is: ~s~%" (get-state-name 42))

(define *flickr-API-key* "d964b85147ddd4082dc029f371fe28a8")

(define flickr (xmlrpc-server "api.flickr.com" 80 "/services/xmlrpc"))
(define flickr.photos.search (flickr "flickr.photos.search"))
(define ht (make-hash-table))
(hash-table-put! ht 'api_key *flickr-API-key*)
(define cat-photos (flickr.photos.search
                    ht

                    ;; eventually I should include a tag argument
                    ;; here, but first I want to understand why it's
                    ;; failing

                    ))
(printf "Cat photos: ~s~%" cat-photos)
)