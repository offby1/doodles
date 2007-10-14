#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui lazy-photo-stream-tests 'verbose))"
|#
(module lazy-photo-stream (lib "lazy.ss" "lazy")
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "get-one-batch.ss")

(define (get-all-photos first-page)
  ;; note that this takes *everything*, to infinity (not beyond)
  (let ((b (get-one-batch #:page first-page)))
    (if (null? b)
        b
        (append b (get-all-photos (add1 first-page))))))

(define photo-stream (get-all-photos 1))

(provide photo-stream)


(define lazy-photo-stream-tests

  (test-suite
   "lazy-photo-stream"
   (test-case
    "yow"
    (check-regexp-match
     #rx"bar"
     "foo"))))


)
