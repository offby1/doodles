#lang scheme

(require (only-in (planet lizorkin/ssax/ssax) ssax:xml->sxml)
         xml)

(define (collapse-adjacent-strings seq)
  (if (list? seq)
      (reverse
       (for/fold ([result '()])
           ([elt (in-list seq)])
           (if (and (not (null? result))
                    (string? (car result))
                    (string? elt))
               (cons (string-append (car result)
                                    elt)
                     (cdr result))
               (cons elt result))))
      seq))

(define *input* (string-append "<snord foo=\"bar\">Plop &amp; Flop<child>flip &amp; flop</child></snord> "))
(printf "From SSAX: ~s~%" (call-with-input-string *input* (lambda (ip) (ssax:xml->sxml ip '()))))
(printf "From XML : ~s~%"  (collapse-adjacent-strings (call-with-input-string *input* (lambda (ip) (xml->xexpr (document-element (read-xml ip)))))))
