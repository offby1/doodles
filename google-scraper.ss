#lang scheme

;; Here's some silliness.  I want to read HTML from a web site
;; (Google), and use sxpath to easily search through it.  Sxpath
;; requires sexps in a certain format, and the easy way to get that
;; format is to use the "htmlprag" package ... alas, as of August
;; 2008, that package doesn't work with PLT v4.  So instead, I use
;; PLT's built-in "read-html-as-xml", then _write that back out as
;; XML_, and then parse that XML with SSAX.  _Then_ I can use sxpath
;; on it.

(require (planet "sxml.ss" ("lizorkin" "sxml.plt"))
         (planet "ssax.ss" ("lizorkin" "ssax.plt"))
         xml
         html
         net/url)

(define (tree-search t predicate)
  (let ((finder (lambda (p) (and (predicate p) p))))
    (cond
     ((pair? t)
      (or (finder t)
          (tree-search (car t) finder)
          (tree-search (cdr t) finder)))
     (else
      #f))))

(let ((ofn  "ooh-baby"))
  (call-with-output-file ofn
    (lambda (file-op)
      (let ((url (string->url "http://www.google.com/search")))
        (set-url-query! url `((q
                               .
                               ,(string-join (list "cats") " "))))
        (let-values (((ip op) (make-pipe)))
          (thread
           (lambda ()
             (for/list ((content (in-list (read-html-as-xml
                                           ;;(open-input-file "test-data.html")
                                           (get-pure-port url)
                                           ))))
               (write-xml/content content op)
               (flush-output-port op))))
          (display (ssax:xml->sxml ip '()) file-op)
          (newline file-op))))

    #:exists 'truncate)
  (fprintf (current-error-port)  "Yer results are in ~s~n" ofn))
