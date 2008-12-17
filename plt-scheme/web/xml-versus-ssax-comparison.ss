#lang scheme

(require (only-in (planet lizorkin/ssax/ssax) ssax:xml->sxml)
         xml)

(define *input* (string-append "<snord foo=\"bar\">Plop &amp; Flop</snord> "))
(printf "From SSAX: ~s~%" (call-with-input-string *input* (lambda (ip) (ssax:xml->sxml ip '()))))
(printf "From XML: ~s~%"  (call-with-input-string *input* (lambda (ip) (xml->xexpr (document-element (read-xml ip))))))
