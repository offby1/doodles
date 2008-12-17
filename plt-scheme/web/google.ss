#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

;; simple command-line thing that does a Google search.  Since Google
;; doesn't seem to have a search API, we gotta "scrape" the returned
;; HTML.

(module google mzscheme
(require (only (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
               sxpath)
         (only (planet ashinn/html-parser:1:1/html-parser) html->sxml)
         (only (lib "pretty.ss")
               pretty-print)
         (lib "url.ss" "net")
         (only (lib "13.ss" "srfi")
               string-join))

(provide main)
(define (main . args)
  (let ((url (string->url "http://www.google.com/search")))
    (set-url-query! url `((q
                           .
                           ,(string-join (vector->list (current-command-line-arguments)) " "))))

    (let ((result (call/input-url
                   url
                   get-pure-port
                   html->sxml)))
      (printf "~s~%" result)

      (let* ((class-g-anchors ((sxpath

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
                            class-g-anchors)))
        (for-each (lambda (caption link)
                  (printf "~s ~s~%" caption link))
                captions
                hrefs))
      )))

)
