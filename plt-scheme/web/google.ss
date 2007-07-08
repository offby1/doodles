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

(let* ((url (make-url "http"                    ;scheme
                      #f                        ;user
                      "www.google.com"          ;host
                      #f                        ;port
                      #t                        ;path-absolute?
                      (list (make-path/param "search" '())) ;path
                      (list '(q . "kitty cats"))            ;query
                      #f                                    ;fragment
                      ))
       (result  (html->shtml
                 (port->string (get-pure-port
                                url
                                (list)))))

       (links  ((sxpath '("//div[@class=\"g\"]" h2 a @ href)) result))
       )

  (pretty-print links)
  )
)