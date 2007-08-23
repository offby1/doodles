#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui headline-tests 'verbose))"
|#
(module headline mzscheme
(require (lib "trace.ss")
         (only (lib "19.ss" "srfi") time?)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "assert.ss"  ("offby1"     "offby1.plt"))
         (only (planet "memoize.ss" ("dherman" "memoize.plt" )) define/memo*)
         "globals.ss"
         "tinyurl.ss")

(register-version-string "$Id$")

;; this is what our queue returns.
(define-struct entry (timestamp title link) (make-inspector))
(define (public-make-entry time title link)
  (check-type 'make-entry time? time)
  (check-type 'make-entry string? title)
  (check-type 'make-entry string? link)
  (make-entry time title link))

;; memoize this so that we don't hit tinyurl.com more than we have to.
(define/memo* (maybe-make-URL-tiny e)
  (let ((new-entry (apply make-entry (cdr (vector->list (struct->vector e))))))
    (let ((original-url (entry-link new-entry)))
      (when (< (*tinyurl-url-length-threshold*) (string-length original-url))
          (set-entry-link! new-entry (make-tiny-url original-url)))
      new-entry)))

(define headline-tests

  (let ((short (make-entry 'irrelevant "irrelevant" "http://short.com"))
        (long  (make-entry 'irrelevant "irrelevant" long-url)))
    (test-suite
     "headline"
     (test-case
      "maybe-make-URL-tiny"
      (with-handlers
          ([exn:fail:network?
            (lambda (e)
              (fprintf (current-error-port)
                       "Can't contact tinyurl; skipping the test~%"))])
        (check-regexp-match  #rx"http://tinyurl.com/....."
                             (entry-link (maybe-make-URL-tiny long)))))
     (test-case
      "leaves short ones alone"
      (check-equal? (entry-link (maybe-make-URL-tiny short))
                    "http://short.com"))
     )))

(provide (all-defined-except make-entry)
         (rename public-make-entry make-entry))
)
