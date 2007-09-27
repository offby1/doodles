#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui line-break-tests 'verbose))"
|#
(module line-break mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "assert.ss"   ("offby1" "offby1.plt"))
         (only (lib "13.ss" "srfi")
               string-join
               string-tokenize))

;; written in a hurry for some guy on IRC

(define *max-line-length* 43)

(define (maybe-space str)
  (if (positive? (string-length str))
      " "
    ""))

(define (break-into-lines str)
  (let loop ((tokens (string-tokenize str))
             (current-line "")
             (result '()))
    (cond
     ((null? tokens)
      (reverse (if (positive? (string-length current-line))
                   (cons current-line result)
                 result)))

     ;; If putting this token on the current line would make it too
     ;; long, then start a new line.
     ((< *max-line-length*
         (+ (string-length (maybe-space current-line))
            (string-length (car tokens))
            (string-length current-line)))
      (loop (cdr tokens)
            (car tokens)
            (cons current-line result)))
     (else
      (loop (cdr tokens)
            (string-append current-line
                           (maybe-space current-line)
                           (car tokens))
            result)))))


(define line-break-tests

  (test-suite
   "line-break"
   (test-case
    "yow"
    (let* ((desired
            (list
             "when the chick came from beach, got her"
             "launch and got back to it cose the wheather"
             "was really nice, so she got some fruit and"
             "water because it was really boiling, and"
             "that's all, she just got back, what more"
             "you want me to say? oh my"))
           (unbroken (string-join desired " ")))
      (check-equal? (break-into-lines unbroken) desired)))))

(provide (all-defined))
)
