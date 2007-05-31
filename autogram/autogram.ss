#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#


(module autogram mzscheme

(require
 (only (lib "1.ss" "srfi")
       filter
       fold)
 (planet "memoize.ss" ("dherman" "memoize.plt" 2 1))
 (planet "numspell.ss" ("neil" "numspell.plt" 1 0))
 "hash-counter.ss")

(define a-template (list
                    "This sentence contains "
                    (cons #\a 0)
                    " as well as "
                    (cons #\b 0)

                    ", " (cons #\e 0)
                     ", " (cons #\o 0)
                     ", " (cons #\t 0)
                    ", and " (cons #\z 0)
                    "."
                    ))

(define (maybe-pluralize s n)
  (if (= n 1)
      s
    (string-append s "s")))

(define (update-template t counts)
  (reverse
   (fold (lambda (thing so-far)
           (if (string? thing)
               (cons thing so-far)
             (cons (cons  (car thing) (get-count (car thing) counts))
                   so-far)))
         '()
         t)))

(define (randomly-seed t)
  (reverse
   (fold (lambda (thing so-far)
           (if (string? thing)
               (cons thing so-far)
             (cons (cons (car thing)
                         (random 100))
                   so-far)))
         '()
         t)))

(define/memo* (survey s)
  (let ((counts (make-count)))
    (let loop ((chars-examined 0))
      (if (= chars-examined (string-length s))
          counts
        (let ((c (string-ref s chars-examined)))
          (when (char-alphabetic? c)
            (inc-count! (char-downcase c) counts))

          (loop (add1 chars-examined)))))))

;; memoization seems pointless here, since if we're searching for
;; truths, we should never call this twice on the same template.
(define (template->counts t)
  (fold
   add-counts
   (make-count)
   (map survey
        (template->strings t))))

(define (template->strings t)
  (reverse
   (fold (lambda (thing so-far)
           (if (string? thing)
               (cons thing so-far)
             (let ((n (cdr thing)))
               (cons (string-append
                      (number->english n)
                      " "
                      (maybe-pluralize
                       (string-append "'"
                                      (make-string 1 (car thing))
                                      "'")
                       n))
                     so-far))))


         '()
         t)))

(define (just-the-conses seq)
  (filter pair? seq))

(define (make-calm-notifier proc)
  (define (is-power-of-two? x)
    (or (= 1 x)
        (and (not (odd? x))
             (is-power-of-two? ( / x 2)))))
  (let ((invocation-count 0))
    (lambda args
      (set! invocation-count (add1 invocation-count))
      (if (is-power-of-two? invocation-count)
          (apply proc args)))))

(let ((worker
       (thread (lambda ()
                 (let ((announce-progress
                        (make-calm-notifier
                         (lambda (t) (fprintf (current-error-port) "~s~%" t)))))
                   (let loop ((t a-template))
                     (announce-progress t)
                     (let* ((t-counts (template->counts t))
                            (next (update-template t t-counts))
                            (n-counts (template->counts next)))
                       (if (counts-equal? t-counts n-counts (map car (just-the-conses t)))
                           (printf "We got a winner: ~s~%" (apply string-append (template->strings t)))
                         (loop  (update-template t (random-progress t-counts n-counts)))))))))))

  (let ((seconds-to-run 10))
    (when (not (sync/timeout seconds-to-run worker))
      (fprintf (current-error-port)
               "~a seconds have elapsed; quitting~%"
               seconds-to-run)
      (kill-thread worker))))
)

