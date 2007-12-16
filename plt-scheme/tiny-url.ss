;; a web server which acts as a function that maps one URL to another.

;; the second URL has this form: http://whatever/XXXXXX

;; "whatever" is the web server's own root.  XXXXXX is six
;; alphanumeric characters.

;; naturally the resultant URL, when we "get" it, redirects us to the
;; original input URL.


;; TODO

;; arrange things ... somehow ... so that we don't need to pass a
;; "get" query.

(module tiny-url mzscheme
(require (lib "errortrace.ss" "errortrace")
         (planet "digest.ss" ("soegaard" "digest.plt" 1))
         (lib "url.ss" "net")
         (lib "servlet.ss" "web-server"))

(provide interface-version timeout start)

(define interface-version 'v1)
(define timeout +inf.0)

(define *inverses* (make-hash-table 'equal))
(define (hash-and-store string)
  (let ((rv
         (substring (sha1 (string->bytes/utf-8 string))
                    0
                    5)))
    (hash-table-put! *inverses* rv string)
    rv))
(define (invert string)
  (hash-table-get *inverses* string #f))

(define (start initial-request)

  `(html
    (body

     ,(format "~s => ~s~%"
              initial-request
              (cond
               ((assq 'make (request-bindings initial-request))
                =>
                (lambda (pair)
                  (hash-and-store (cdr pair))))
               ((assq 'get (request-bindings initial-request))
                =>
                (lambda (pair)
                  (invert (cdr pair))))
               (else
                "gaah"))))))

)
