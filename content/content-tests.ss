#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require (planet "test.ss"    ("schematics" "schemeunit.plt" ))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" ))
         (planet "hash-store.ss" ("jaymccarthy" "hash-store.plt"))
         scheme/port
         "content.ss")

(define tests
  (test-suite
   "yow"
   (test-case
    "trivial"
    (let ((s (make-store)))
      (after
       (check-exn exn:fail:hash-store:exists? (lambda () (get s #"123")))
       (check-equal? (get s (put! s #"Snarkulous")) #"Snarkulous")
       (printf "Here 'tis: ~s~%" s)
       (check-exn exn:fail:hash-store:exists?  (lambda () (get s #"987")))
       (put! s #"Snarkulous")
       (nuke! s))))

   (test-case
    "directory contents"
    (printf "Here is this directory's files: ~s~%"
            (let ((store (make-store)))
              (for ([file (directory-list)])
                (when (file-exists? file)
                  (put! store
                        (call-with-input-file
                            file
                          (lambda (ip)
                            (read-bytes (file-size file) ip))))))
              store)))))

(provide  main)
(define (main . args)
  (exit (test/text-ui tests 'verbose)))