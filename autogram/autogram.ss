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
 "hash-counter.ss"
 "odometer.ss")

(define a-template (list
                    "Brad Srebnik wants you to know that this sentence contains "
                    (cons #\a 1)
                    ", " (cons #\b 1)
                    ", " (cons #\e 1)
                    ", " (cons #\o 1)
                    ", " (cons #\i 1)
                    " and " (cons #\t 1)
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
  (let ((rv (make-count)))
    (for-each
     (lambda (thing)
       (add-counts! rv (survey thing)))
     (template->strings t))
    rv))
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
;(trace template->strings)
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
     (nl (current-error-port))
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
(port-count-lines! (current-error-port))

(define (nl p)
  (let-values (((line col pos)
                (port-next-location p)))
    (unless (zero? col)
      (newline p))))

(define (increment-template t)
  (let ((new-nums (increment (map cdr (just-the-conses t)) 1 8)))
    (when (not new-nums)
      (printf "Uh oh, maxed out.")
      (kill-thread (current-thread)))
    (let loop ((conses (map (lambda (old-pair new-num)
                              (cons (car old-pair)
                                    new-num))
                            (just-the-conses t) new-nums))
               (t t)
               (new '()))
      (if (null? t)
          (reverse new)
        (loop (if (pair? (car t)) (cdr conses) conses)
              (cdr t)
              (cons (if (pair? (car t)) (car conses) (car t)) new))))))
;(trace increment-template)
(let ((worker
       (thread (lambda ()
                 (let loop ((t a-template))
                   (announce-progress t)
                   (let* ((the-conses (just-the-conses t))
                          (t-counts (template->counts t))
                          (next (update-template-from-counts t t-counts))
                          (n-counts (template->counts next)))
                     ;;(printf "'next' is ~s~%" next)
                     (set! *tries* (add1 *tries*))
                     (if (counts-equal? t-counts n-counts (map car the-conses))
                         (printf "We got a winner: ~s -> ~s~%"
                                 next
                                 (apply string-append (template->strings next)))
                       (begin
                         (set! *distinct-variants-seen* (add1 *distinct-variants-seen*))
                         (loop
                          (increment-template t)))))))))

      (monitor (thread
                ;; this seems overly complex.
                (lambda ()
                  (let loop ((previous-tries #f)
                             (tries *tries*)
                             (last-sample-time #f)
                             (now (current-process-milliseconds)))
                    (nl (current-error-port))
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

  (let ((seconds-to-run 600))
    (when (not (sync/timeout seconds-to-run worker))
      (fprintf (current-error-port)
               "~a seconds have elapsed; quitting "
               seconds-to-run
               )
      (kill-thread worker))
    (fprintf (current-error-port) "after ~a tries~%" *tries*)
    (kill-thread monitor)))
)
