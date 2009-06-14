#lang scheme

(require (planet ashinn/html-parser:1:0/html-parser)
         net/url)

(call-with-output-file "ooh-baby"
  (lambda (file-op)
    (let ((url (string->url "http://www.google.com/search")))
      (set-url-query! url `((q . ,(string-join (list "cats") " "))))
      (call/input-url
       url
       get-pure-port
       (lambda (ip)
         (pretty-print (html->sxml ip)  file-op))))
    (fprintf (current-error-port)  "Yer results are in ~s~n" file-op))

  #:exists 'truncate)
