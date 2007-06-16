#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
ulimit -d 400000
exec mzscheme -qu "$0" ${1+"$@"}
|#


(module autogram mzscheme

(require
 (only (lib "1.ss" "srfi")
       every
       filter
       fold
       iota)
 (lib "date.ss")
 (lib "trace.ss")
 (lib "round.scm" "offby1")
 (planet "memoize.ss" ("dherman" "memoize.plt" 2 1))
 (planet "numspell.ss" ("neil" "numspell.plt" 1 0))
 "byte-vector-counter.ss"
 "num-string-commas.ss"
 "monitor.ss"
 "globals.ss"

 )

(define *a-template* (cons "I'd gladly pay you Tuesday for a hamburger today, along with "
                      (fold (lambda (pair seq)
                             (if (not (null? seq))
                                 (cons pair (cons
                                             (if (null? (cdr seq))
                                                 ", and "
                                               ", ")
                                             seq))
                               (cons pair seq)))
                           '()
                           (map (lambda (i)
                                  (cons ( integer->char i) 1))
                                (reverse
                                 (iota (add1 (- (char->integer #\z) (char->integer #\a)))
                                       (char->integer #\a)))))))

(define (just-the-conses seq) (filter pair? seq))
(define (maybe-pluralize s n)
  (if (= n 1)
      s
    (string-append s "'s")))

(define (modify-template t number-proc)
  (let ((rv
         (reverse
          (fold (lambda (thing so-far)
                  (if (string? thing)
                      (cons thing so-far)
                    (cons (cons  (car thing) (number-proc thing))
                          so-far)))
                '()
                t))))
    rv))

(define (update-template-from-counts t counts)
  (modify-template t (lambda (pair) (get-count (car pair) counts))))

;;(trace update-template-from-counts)

(define/memo* (survey s)
  (let ((counts (make-count)))
    (let loop ((chars-examined 0))
      (if (= chars-examined (string-length s))
          counts
        (let ((c (char-downcase (string-ref s chars-examined))))
          (when (char-alphabetic? c)
            ;;(set! c (char-downcase c))
            (inc-count! c counts))

          (loop (add1 chars-examined)))))))
;;(trace survey)

(define/memo* (pair->string p)
  (let ((n (cdr p)))
    (string-append
     (number->english n)
     " "
     (maybe-pluralize
      (string-append (make-string 1 (car p)) )
      n))))

;; memoization seems pointless here, since if we're searching for
;; truths, we should never call this twice on the same template.
(define (template->strings t)
  (reverse
   (fold (lambda (thing so-far)
           (if (string? thing)
               (cons thing so-far)
             (cons (pair->string thing) so-far)))
         '()
         t)))
;;(trace template->strings)

;; ditto about the pointlessness of memoization
(define (template->counts t)
  (let ((rv (make-count)))
    (for-each
     (lambda (str)
       (add-counts! rv (survey str)))
     (template->strings t))
    rv))
;;(trace template->counts)

(define (true? t actual-counts)

  (every (lambda (pair)
           (= (cdr pair)
              (get-count (car pair) actual-counts)))
         (just-the-conses t)))
;;(trace true?)

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

(define testing-truth-progress
  (make-calm-notifier
   (lambda (t)
     (nl)
     (printf
      "~a ~s~%"
      (parameterize ((date-display-format 'iso-8601))
                    (date->string (seconds->date (current-seconds)) #t))
      (apply string-append (template->strings t))))))

(define *seen* (make-hash-table 'equal))
(define (already-seen? counts)
  (hash-table-get *seen* counts #f))
;;(trace already-seen?)
(define note-seen!
  (let ((number-seen 0))
    (lambda ( counts)
      (set! number-seen (add1 number-seen))
      (hash-table-put! *seen* counts #t))))
;;(trace note-seen!)
(define (randomize-template t)
  (modify-template t (lambda (ignored)
                       (+ *min* (random (- *max* *min* -1))))))
;;(trace randomize-template)
(define monitor-thread
  (thread (lambda ()
            (monitor (expt (- *max* *min* -1) (length (just-the-conses *a-template*)))))))
(call/ec
 (lambda (return)

   (parameterize-break
    #t
    (with-handlers
        ([exn:break?
          (lambda (x)
            (printf "I finally got a break!~%")
            (return))])

      (let ((worker
             (thread
              (lambda ()
                (let loop ((t *a-template*))
                  (*loop-passes* (add1 (*loop-passes*)))
                  (let ((actual-counts (template->counts t))
                        (claimed-counts (apply make-count (map cdr (just-the-conses t)) )))
                    (if (already-seen? claimed-counts)
                        (loop (randomize-template t))
                      (begin
                        (*tries* (add1 (*tries*)))
                        (testing-truth-progress t)
                        (note-seen! claimed-counts)
                        (when (true? t actual-counts)
                          (printf "We got a winner: ~s~%"
                                  (apply string-append (template->strings t))))
                        (loop
                         (update-template-from-counts t actual-counts))))))))))


        (printf "Well, here we go.~%")
        (sync worker)
        (fprintf (current-error-port) "after ~a tries~%" (num-string-commas (*tries*))))))))
(once-more-and-then-quit)
(sync monitor-thread)
)
