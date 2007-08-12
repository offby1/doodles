#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui del.icio.us-tests 'verbose))"
|#

(module del mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (lib "cmdline.ss")
         (planet "delicious.ss" ("untyped" "delicious.plt" 1 1))
         (only (lib "19.ss" "srfi")
               date->string)
         (only (lib "1.ss" "srfi")
               every
               take)
         (lib "pretty.ss")
         (lib "trace.ss")
         "globals.ss"
         "headline.ss")

;; return all items with the tag "moviestowatchfor"

;;(dump-sxml-responses? #t)

(define (snarf-em-all)
  (parameterize
      ((current-password
        (or (*del.icio.us-password*)

            ;; more convenient for testing
            (getenv "DELICIOUS_PASSWORD")

            "unknown")
        )
       (current-username "tucumcari"))

    (let ((gotten (recent-posts "moviestowatchfor")))
      (display "Got these:")
      (pretty-print gotten)
      (map (lambda (post)
             (make-entry (post-date post)
                         (post-description post)
                         (post-url post))
             ) gotten))))

(define del.icio.us-tests

  (test-suite
   "del.icio.us"

   (test-not-false
    "gets some movies, and they're all entries"
    (with-handlers ([exn:delicious:auth?
                     (lambda (e)
                       (fprintf
                        (current-error-port)
                        "wrong delicious password; skipping the test~%")
                       #t)])
      (let ((snarfage (snarf-em-all)))
        (check-false     (null? snarfage) "didn't return any entries")
        (check-not-false (every entry? snarfage) "They're not all entries")))
    )))
(provide (all-defined))
)