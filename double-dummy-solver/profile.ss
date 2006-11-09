#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require (lib "errortrace.ss" "errortrace")
         (only (lib "etc.ss") this-expression-source-directory)
         (only (lib "list.ss") sort)
         (only (lib "1.ss" "srfi") filter))

(profiling-enabled #t)
(profiling-record-enabled #t)
(execute-counts-enabled #t)
(profile-paths-enabled #t)

(require "filler.ss")

;; for emacs
;; (put 'mit-clobbering 'scheme-indent-function (get 'with-output-to-file 'scheme-indent-function))
(define (mit-clobbering string thunk)
  (when (file-exists? string)
    (delete-file string))
  (with-output-to-file string
    thunk))

(let* ((here (this-expression-source-directory))
       (od (simplify-path (build-path here "coverage"))))
  (unless (directory-exists? od)
    (make-directory od))
  (for-each (lambda (fn)
              (let ((ofn  (build-path od fn)))
                (mit-clobbering ofn
                  (lambda ()
                    (annotate-executed-file fn)))))
            (filter (lambda (path)
                      (and (file-exists? path)
                           (regexp-match "\\.ss$" (path->string path))))
                    (directory-list here)))
  (for-each
   (lambda (p)
     (let ((ofn (simplify-path (build-path od (car p)))))
       (mit-clobbering ofn
         (cdr p))
       (fprintf (current-error-port)
                "Wrote ~a.~%" ofn))
     )
   `(("profile-stuff"
      . ,(lambda ()
           (printf "Hey Emacs, -*- coding:utf-8 -*- rocks!~%")
           (for-each (lambda (datum)
                       (apply
                        (lambda (called milliseconds name source paths)
                          (printf "time = ~a : no. = ~a : µs per call = ~a : ~a ~a~%"
                                  milliseconds
                                  called
                                  (if (or (zero? called))
                                      +inf.0
                                    (exact->inexact
                                     (/ (truncate (* 10000
                                                     (/ milliseconds called)))
                                        10)))
                                  name
                                  source)
                          (for-each (lambda (path)
                                      (printf "   ~a~%" (car path))
                                      (for-each (lambda (location)
                                                  (printf "      ~a~%" location))
                                                (cdr path)))
                                    (sort paths (lambda (a b)
                                                  (> (car a)
                                                     (car b))))))
                        datum))
                     (sort (get-profile-results)
                           (lambda (a b)
                             (< (car a)
                                (car b)))))
           ))
     ("README"
      . ,(lambda () (printf "Key to the code-coverage symbols:~%^: 0~%.: 1~%,: >1~%"))))))