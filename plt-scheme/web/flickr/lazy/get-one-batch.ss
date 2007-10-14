#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui get-one-batch-tests 'verbose))"
|#
(module get-one-batch mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (lib "kw.ss")
         (only (lib "1.ss" "srfi") iota))

;; returns a simple list of photos, not an actual page.
(define/kw (get-one-batch  #:key
                           [page 1]
                           [per_page 3])
  (fprintf (current-error-port)
           "Getting at most ~a photos from page ~a~%"
           per_page page)
  (map
   (lambda (n)
     `(photo
       (@ (title "Yours Truly")
          (server "2305")
          (secret "c8c4e9bf53")
          (owner "20825469@N00")
          (ispublic "1")
          (isfriend "0")
          (isfamily "0")
          (id ,(number->string n))
          (farm "3"))))
   (iota per_page (* (sub1 page) per_page))))

(define get-one-batch-tests

  (test-suite
   "get-one-batch"
   (test-case
    "yow"
    (check-regexp-match
     #rx"bar"
     "foo"))))

(provide (all-defined))
)
