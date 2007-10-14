(require (planet "flickr.ss" ("dvanhorn" "flickr.plt" 1 0)))
(current-api-key "d964b85147ddd4082dc029f371fe28a8")
(current-sec-key (or (getenv "FLICKR_SECRET") ""))
(sign-all? #t)
(printf "current-sec-key is ~s~%" (current-sec-key))
(printf "An echo test: ~s~%" (flickr.test.echo
                              #:foo "bar"))
(printf "An auth frob: ~s~%" (flickr.auth.getFrob))
