#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module google mzscheme
(require (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
         (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
         (planet "port.ss"      ("schematics"  "port.plt" ))
         (lib "pretty.ss")
         (lib "url.ss" "net"))

(define (grab-as thing)
  (cond
   ((and (pair? thing)
         (eq? 'a (car thing)))
    (list thing))
   ((pair? thing)
    (append (grab-as (car thing))
            (grab-as (cdr thing))))
   (else
    '())
   ))
(for-each
 (lambda (input)
   (printf "~s => ~s~%" input (grab-as input)))
 (list '()
       "just a string"
       '(a "A single a")
       '((a "First of a list of as")
         (a "Second of a list"))
       '((a "First of a list of as with intervening cruft")
         'intervening
         "cruft"
         (a "Second of a list with intervening cruft"))))

(let* ((url (make-url "http"                    ;scheme
                      #f                        ;user
                      "www.google.com"          ;host
                      #f                        ;port
                      #t                        ;path-absolute?
                      (list (make-path/param "search" '())) ;path
                      (list'(q . "money"))                  ;query
                      #f                                    ;fragment
                      ))
       (result  (html->shtml
                 (port->string (get-pure-port
                                url
                                (list))))))

  ;; display all the "a" elements in the result, along with their "path"s.
  (pretty-display (grab-as result))
  )
)