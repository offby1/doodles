#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; simple command-line thing that does a Google search.  Since Google
;; doesn't seem to have a search API, we gotta "scrape" the returned
;; HTML.

(module google mzscheme
(require (only (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
               sxpath)
         (only (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
               html->shtml)
         (only (planet "port.ss"      ("schematics"  "port.plt" ))
               port->string)
         (only (lib "pretty.ss")
               pretty-print)
         (only (lib "url.ss" "net")
               make-url
               make-path/param
               get-pure-port)
         (only (lib "13.ss" "srfi")
               string-join))

(let* ((url (make-url "http"                    ;scheme
                      #f                        ;user
                      "www.google.com"          ;host
                      #f                        ;port
                      #t                        ;path-absolute?
                      (list (make-path/param "search" '())) ;path
                      (list
                       `(q .
                           ,(string-join (vector->list (current-command-line-arguments)) " ")
                           ))                               ;query
                      #f                                    ;fragment
                      ))
       (result  (html->shtml
                 (port->string (get-pure-port
                                url
                                (list)))))

       (class-g-anchors ((sxpath

                  ;; in English: get all the "div"s that have class
                  ;; "g", and then from each, return all the anchors.

                  ;; there's no guarantee that this will continue to
                  ;; work; Google may well reformat their results any
                  ;; time they feel like it.  I'd use a search API
                  ;; (instead of "scraping" the html as I'm doing here)
                  ;; if they offered one.
                  '(// (div (@ (equal? (class "g")))) h2 a)) result))
       (hrefs    ((sxpath '(@ href *text*)) class-g-anchors))
       (captions (map (lambda (a)
                        (apply string-append ((sxpath '(// *text*)) a)))
                      class-g-anchors))
       )

  (for-each (lambda (caption link)
              (printf "~s ~s~%" caption link))
            captions
            hrefs))
)
