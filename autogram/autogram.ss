#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#


(module autogram mzscheme

(require
 (only (lib "1.ss" "srfi")
       filter
       fold)
 (lib "date.ss")
 (lib "trace.ss")
 (planet "memoize.ss" ("dherman" "memoize.plt" 2 1))
 (planet "numspell.ss" ("neil" "numspell.plt" 1 0))
 "hash-counter.ss")

(define a-template (list
                    "Brad Srebnik wants you to know that this sentence contains "
                    (cons #\b 2)
                    ", " (cons #\c 3)
                    ", " (cons #\d 3)
                    ", " (cons #\f 1)
                    ", " (cons #\g 3)
                    ", " (cons #\h 10)
                    ", " (cons #\i 7)
                    ", " (cons #\j 1)
                    ", " (cons #\k 3)
                    ", " (cons #\l 1)
                    ", " (cons #\m 1)
                    ", " (cons #\n 23)
                    ", " (cons #\o 16)
                    ", " (cons #\p 1)
                    ", " (cons #\q 1)
                    ", " (cons #\r 8)
                    ", " (cons #\s 22)
                    ", " (cons #\t 28)
                    ", " (cons #\u 2)
                    ", " (cons #\v 2)
                    ", " (cons #\w 10)
                    ", " (cons #\x 0)
                    ", " (cons #\y 0)
                    ", and " (cons #\z 1)
                    "."))

(define (maybe-pluralize s n)
  (if (= n 1)
      s
    (string-append s "s")))

(define (modify-template t proc)
  (let ((rv
         (reverse
          (fold (lambda (thing so-far)
                  (if (string? thing)
                      (cons thing so-far)
                    (cons (cons  (car thing) (proc thing))
                          so-far)))
                '()
                t))))
    rv))

(define (randomize-template t)
  (modify-template t (lambda (pair) (random 100))))
;(trace randomize-template)

(define (update-template-from-counts t counts)
  (modify-template t (lambda (pair) (get-count (car pair) counts))))

;(trace update-template-from-counts)

(define/memo* (survey s)
  (let ((counts (make-count)))
    (let loop ((chars-examined 0))
      (if (= chars-examined (string-length s))
          counts
        (let ((c (string-ref s chars-examined)))
          (when (char-alphabetic? c)
            ;;(set! c (char-downcase c))
            (inc-count! c counts))

          (loop (add1 chars-examined)))))))
;(trace survey)

;; memoization seems pointless here, since if we're searching for
;; truths, we should never call this twice on the same template.
(define (template->counts t)
  (fold
   add-counts
   (make-count)
   (map survey
        (template->strings t))))
;(trace template->counts)

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

(define announce-progress
  (make-calm-notifier
   (lambda (t)
     (fprintf
      (current-error-port)
      "~a ~s~%"
      (parameterize ((date-display-format 'iso-8601))
                    (date->string (seconds->date (current-seconds)) #t))
      (apply string-append (template->strings t))))))

;; this will be written to by the worker thread, and read from by the
;; main thread.  That may not be thread-safe, but nothing awful will
;; happen if it gets corrupted (its value is only used for progress
;; messages), and anyway I can't figure out the right thread-safe way
;; to manipulate it.

(define *tries* 0)
(define *distinct-variants-seen* 0)

(let ((worker
       (thread (lambda ()
                 (let loop ((t a-template)

                            (paths-started 0)

                            ;; if we keep calling update-template,
                            ;; eventually we'll loop: imagine we go
                            ;; from node A to B, C, D, E, F, and then
                            ;; C.  This keeps track of the length of
                            ;; the _entire_ path, including the
                            ;; initial tail -- in our example it'd be
                            ;; 6.
                            (current-path-length 0)

                            (seen (make-hash-table 'equal)))
                   (announce-progress t)
                   (let* ((the-conses (just-the-conses t))
                          (t-counts (template->counts t))
                          (next (update-template-from-counts t t-counts))
                          (n-counts (template->counts next)))
                     (set! *tries* (add1 *tries*))
                     (if (counts-equal? t-counts n-counts (map car the-conses))
                         (printf "We got a winner: ~s~%" (apply string-append (template->strings next)))
                       (let ((encountered-previously (hash-table-get seen n-counts #f)))
                         (if encountered-previously
                             (begin
                               (if (= paths-started (car encountered-previously))
                                   (printf "Oops: a loop of length ~a~%"
                                           (- current-path-length (cdr
                                                                   encountered-previously)))
                                 (begin
                                   (printf ".")
                                   (flush-output)))
                               (loop  (randomize-template next)
                                      (add1 paths-started)
                                      0

                                      ;; starting with a new hash
                                      ;; table keeps us from consuming
                                      ;; too much memory ... but we
                                      ;; tend to hit the same loops
                                      ;; over and over ...
                                      seen
                                      ;;(make-hash-table 'equal)
                                      ))
                           (begin
                             (set! *distinct-variants-seen* (add1 *distinct-variants-seen*))
                             (hash-table-put!
                              seen
                              n-counts
                              (cons
                               paths-started
                               current-path-length))
                             (loop
                              next
                              paths-started
                              (add1 current-path-length)
                              seen))))))))))
      (monitor (thread
                ;; this seems overly complex.
                (lambda ()
                  (let loop ((previous-tries #f)
                             (tries *tries*)
                             (last-sample-time #f)
                             (now (current-process-milliseconds)))
                    (fprintf
                     (current-error-port)
                     "~a tries (~a distinct variants seen) ~a~%"
                     tries
                     *distinct-variants-seen*
                     (if previous-tries
                         (format
                          " (~a tries per second)"
                          (exact->inexact
                           (/ (* 1000 (- tries previous-tries))
                              (max 1 (- now last-sample-time)))))
                       ""))

                    (sleep 30)
                    (loop
                     tries
                     *tries*
                     now
                     (current-process-milliseconds))))
                )))

  (let ((seconds-to-run 10))
    (when (not (sync/timeout seconds-to-run worker))
      (fprintf (current-error-port)
               "~a seconds have elapsed; quitting after ~a tries~%"
               seconds-to-run
               *tries*)
      (kill-thread worker))
    (kill-thread monitor)))
)
