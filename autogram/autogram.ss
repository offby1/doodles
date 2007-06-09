#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#


(module autogram mzscheme

(require
 (only (lib "1.ss" "srfi")
       every
       filter
       fold)
 (lib "date.ss")
 (lib "trace.ss")
 (lib "round.scm" "offby1")
 (planet "memoize.ss" ("dherman" "memoize.plt" 2 1))
 (planet "numspell.ss" ("neil" "numspell.plt" 1 0))
 "hash-counter.ss"
 "odometer.ss"
 "num-string-commas.ss")

(define *timeout-seconds* 3600)
(define *min* 1)
(define *max* 14)
(define *a-template* '("Brad Srebnik wants you to know that this sentence contains "
                       (#\a . 1) ", "
                       (#\b . 1) ", "
                       (#\i . 1) ", "
                       (#\s . 1) " and "
                       (#\t . 1) "."
                       ))

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

(define/memo* (pair->string p)
  (let ((n (cdr p)))
    (string-append
     (number->english n)
     " "
     (maybe-pluralize
      (string-append "'"
                     (make-string 1 (car p))
                     "'")
      n))))

(define (template->strings t)
  (reverse
   (fold (lambda (thing so-far)
           (if (string? thing)
               (cons thing so-far)
             (cons (pair->string thing) so-far)))
         '()
         t)))
;(trace template->strings)

(define (just-the-conses seq)
  (filter pair? seq))

(define (true? t)

  ;; memoization seems pointless here, since if we're searching for
  ;; truths, we should never call this twice on the same template.
  (define (template->counts t)
    (let ((rv (make-count)))
      (for-each
       (lambda (thing)
         (add-counts! rv (survey thing)))
       (template->strings t))
      rv))
  ;;(trace template->counts)

  (let ((actual-counts (template->counts t)))
    (every (lambda (pair)
             (= (cdr pair)
                (get-count (car pair) actual-counts)))
           (just-the-conses t))))
;(trace true?)

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
     (nl)
     (printf
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
(port-count-lines! (current-error-port))
(port-count-lines! (current-output-port))

(define (nl)
  (let-values (((line col pos)
                (port-next-location (current-output-port))))
    (unless (zero? col)
      (newline))))

(define (increment-template t)
  (let ((new-nums (increment (map cdr (just-the-conses t)) *min* *max*)))
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
                 (let loop ((t *a-template*))
                   (announce-progress t)
                   (set! *tries* (add1 *tries*))
                   (if (true? t)
                       (printf "We got a winner: ~s~%"
                               (apply string-append (template->strings t)))
                     (loop
                      (increment-template t)))))))

      (monitor (thread
                ;; this seems overly complex.
                (lambda ()
                  (parameterize
                   ((current-output-port (current-error-port)))
                   (printf "Will bail after ~a tries, or ~a seconds, whichever comes first.~%"
                           (num-string-commas (expt (- *max* *min*) (length (just-the-conses *a-template*))))
                           *timeout-seconds*)
                   (let loop ((previous-tries #f)
                              (tries *tries*)
                              (last-sample-time #f)
                              (now (current-process-milliseconds)))
                     (nl)
                     (printf
                      "~a tries ~a~%"
                      (num-string-commas (my-round tries 2))
                      (if previous-tries
                          (format
                           " (~a tries per second)"
                           (num-string-commas (my-round
                                               (/ (* 1000 (- tries previous-tries))
                                                  (max 1 (- now last-sample-time)))
                                               2)))
                        ""))

                     (sleep 30)
                     (loop
                      tries
                      *tries*
                      now
                      (current-process-milliseconds)))))
                )))

  (let ((seconds-to-run *timeout-seconds*))
    (when (not (sync/timeout seconds-to-run worker))
      (fprintf (current-error-port)
               "~a seconds have elapsed; quitting "
               seconds-to-run
               )
      (kill-thread worker))
    (fprintf (current-error-port) "after ~a tries~%" (num-string-commas *tries*))
    ))
)
