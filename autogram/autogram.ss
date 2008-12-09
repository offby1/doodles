#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5748 2008-11-17 01:57:34Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require
 srfi/1
 (lib "date.ss")
 (except-in (planet offby1/offby1/zdate) main)
 (planet dherman/memoize:2:1/memoize)
 (planet neil/numspell:1:0/numspell)
 (except-in "byte-vector-counter.ss" main)
 "monitor.ss"
 "globals.ss")

;; from "foof" on #scheme (Alex Shinn?)
(define (intersperse-val val lst)
  (cdr (concatenate (zip (circular-list val)
                         lst))))

(define *a-template* `(
                      "What part of '"
                       ,@(intersperse-val
                          ", "
                          (build-list
                           26
                           (lambda (<>)
                             (cons (integer->char
                                    (+ (char->integer #\a) <>))
                                   1))))
                       "' don't you understand?"
                      ))

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

;; ditto about the pointlessness of memoization
(define (template->counts t)
  (let ((rv (make-count)))
    (for-each
     (lambda (str)
       (add-counts! rv (survey str)))
     (template->strings t))
    rv))

(define (true? t actual-counts)

  (every (lambda (pair)
           (= (cdr pair)
              (get-count (car pair) actual-counts)))
         (just-the-conses t)))

(define (make-calm-notifier proc)
  (define (is-power-of-two? x)
    (or (= 1 x)
        (and (not (odd? x))
             (is-power-of-two? ( / x 2)))))
  (let ((invocation-count 0))
    (lambda args
      (set! invocation-count (add1 invocation-count))
      (when (is-power-of-two? invocation-count)
        (apply proc args)))))

(define testing-truth-progress
  (make-calm-notifier
   (lambda (t)
     (nl)
     (printf
      "~a ~s~%"
      (zdate)
      (apply string-append (template->strings t))))))

(define *seen* (make-hash))
(define (already-seen? counts)
  (hash-ref *seen* counts #f))

(define (note-seen! counts)
  (hash-set! *seen* counts #t))

(define (randomize-template t)
  (modify-template t (lambda (ignored)
                       (+ *min* (random (- *max* *min* -1))))))

(provide main)
(define (main . args)
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
                (parameterize
                    ((current-custodian
                      *worker-custodian*))
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
                                 (printf "~%We got a winner: ~s~%~%"
                                         (apply string-append (template->strings t))))
                               (loop
                                (update-template-from-counts t actual-counts)))))))))))


           (printf "Well, here we go.~%")
           (sync worker)
           (fprintf (current-error-port) "after ~a tries~%" (number->english (*tries*))))))))

  (once-more-and-then-quit)
  (sync monitor-thread))
