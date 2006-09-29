#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qr "$0" ${1+"$@"}
|#

(require (lib "errortrace.ss" "errortrace"))

(profiling-enabled #t)
(profiling-record-enabled #t)
;(profile-paths-enabled #t)

(require "card.ss"
         "bridge.ss"
         "history.ss"
         (prefix ha: "hand.ss")
         (lib "pretty.ss")
         (only (lib "list.ss") sort)
         (only (lib "1.ss" "srfi") iota take circular-list))
(define *ranks* 5)                      ;should of course be 13, but
                                        ;... *sigh* ... that's too
                                        ;slow
(define *deck*
  (let loop ((suits *suits*)
             (result '()))
    (if (null? suits)
        (sort result (lambda (a b)
                       (< (card-rank a)
                          (card-rank b))))
      (loop (cdr suits)
            (append
             (let loop ((ranks (iota *ranks* 2))
                        (result '()))
               (if (null? ranks)
                   result
                 (loop (cdr ranks)
                       (cons (make-card (car suits)
                                   (car ranks))
                             result))))
             result)))))

(define (fisher-yates-shuffle! v)
  (define (swap! i1 i2)
    (let ((tmp (vector-ref v i1)))
      (vector-set! v i1 (vector-ref v i2))
      (vector-set! v i2 tmp)))
  (let ((l (vector-length v)))
    (do ((top-index (sub1 l) (sub1 top-index)))
        ((zero? top-index) v)
      (let ((bottom-index (random top-index)))
        (swap! bottom-index top-index)))))

(set! *deck* (vector->list (fisher-yates-shuffle! (list->vector *deck*))))
(define hands (circular-list
               (ha:make-hand '())
               (ha:make-hand '())
               (ha:make-hand '())
               (ha:make-hand '())))

;; deal 'em out
(let loop ((d *deck*)
           (hs hands))
  (unless (null? d)
    (let ((victim (car hs)))
      (ha:add-card! victim (car d)))

    (loop (cdr d)
          (cdr hs))))

;; sort the hands.  This is actually important, since
;; group-into-adjacent-runs will be more likely to return exactly 1
;; group, and hence things will go faster.
(let loop ((h (take hands 4)))
  (unless (null? h)
    (ha:sort! (car h))))

(printf "All hands hold ~a cards.~%"
        (length (apply append  (map ha:cards (take hands 4)))))

(printf "North plays ~s from "
        (choose-card (make-history (list))
                     (take hands 4)
                     0))
(pretty-display (ha:cards (car hands)))
(output-profile-results #t #t)
