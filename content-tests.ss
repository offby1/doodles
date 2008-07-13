#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require (planet "test.ss"    ("schematics" "schemeunit.plt" ))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" ))
         scheme/port
         "content.ss")

(define tests
  (test-suite
   "yow"
   (test-case
    "trivial"
    (let ((s (make-store)))
      (check-false (get s 123))
      (let-values (((s sum) (put s "Snarkulous")))
        (check-equal? (get s sum) "Snarkulous")
        (printf "Here 'tis: ~s~%" s)
        (check-false (get s 987))
        (let-values (((s sum) (put s "Snarkulous")))
          (check-equal? (hash-count s) 1)))))
   (test-case
    "directory contents"
    (printf "Here is this directory's files: ~s~%"
            (for/fold ([store (make-store)])
                      ([file (directory-list)])
                      (if (file-exists? file)
                          (let-values (((s sum)
                                        (put store
                                             (call-with-input-file
                                                 file
                                               (lambda (ip)
                                                 (let ((o (open-output-bytes)))
                                                   (copy-port ip o)
                                                   (get-output-bytes o)))))))
                            s)
                          store)))
    )
   ))

(provide  main)
(define (main . args)
  (exit (test/text-ui tests 'verbose)))