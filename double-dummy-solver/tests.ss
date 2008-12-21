#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

#lang scheme
(require (planet "test.ss"     ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss"  ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"     ("schematics" "schemeunit.plt" 2))
         (only-in (lib "1.ss" "srfi")
               every
               filter
               lset-intersection
               append-map)
         (only-in rnrs/base-6 assert)
         "dds.ss"
         "card.ss"
         "trick.ss"
         (prefix-in ha:  "hand.ss")
         (only-in "hand.ss" mh mhs)
         (except-in "history.ss" whose-turn)
         (lib "trace.ss"))
(display "$Id$" (current-error-port))
(newline (current-error-port))

(print-struct #t)




(let ((first-annotated-card (car (annotated-cards (mt s s2 dt ha)))))

  (check eq? (cdr first-annotated-card) 's)

  (test/text-ui
   (test-suite
    "The one and only suite"

    (test-case
     "Annotations 1"
     (check cards= (car first-annotated-card) (mc s2)))

    (test-case
     "Annotations 2"
     (check eq? (cdr first-annotated-card) 's))

    (test-case
     "follows suit 1"
     (check cards=
            (mc s3)
            (choose-card
             (make-history
              (list (mt e  s2 dt ha)))
             (list (mh n s3 d2))
             *num-ranks*
             #f)))
    (test-case
     "follows suit 2"
     (check eq?
            'd
            (card-suit
             (choose-card
              (make-history
               (list (mt w  dt s2 ha)))
              (mhs (s2 dq d8)
                   (sq dk d7)
                   (s3 d2 d9)
                   (sk dq d6))
              *num-ranks*
              #f))))
    (test-exn "Notices garbage in hand"
              exn:fail:contract?
              (lambda ()
                (choose-card (make-history (car *seats*))
                             (list (list 77))
                             *num-ranks*
                             #f)))
    (test-exn "Notices card in both history and hand"
              exn:fail:contract?
              (lambda ()
                (choose-card
                 (make-history
                  (list (mt n  dt s2 hk)))
                 (list (mh s3 hk))
                 *num-ranks*
                 #f)))
    (test-exn "Can't remove non-existant card from hand"
              exn:fail:contract?
              (lambda ()
                (define h (mh  s2))
                (set! h (ha:remove-card h (mc s2)))
                (set! h (ha:remove-card h (mc s2)))
                ))
    (test-exn "Can't add card twice to hand"
              exn:fail:contract?
              (lambda ()
                (define h (mh  s2))
                (ha:add-card h (mc s2))
                ))

    (test-exn "mt detects duplicate cards"
              exn:fail:contract?
              (lambda ()
                (mt n c6 c9 c3 c3)))

    (test-equal? "Winner 1"
                 'w
                 (winner (mt n c3 c6 c9 cj)))
    (test-equal? "Winner 2"
                 's
                 (winner (mt n c3 c6 cj c9)))
    (test-equal? "Winner 3"
                 'e
                 (winner (mt n c6 c9 c3 dj)))
    (test-equal? "Takes finesses"
                 2
                 (cdr
                  (assq 'n
                        (play-loop
                         (make-history
                          's)
                         (mhs (sq sa)
                              (s2 s3)
                              (s4 s5)
                              (sj sk)

                              )
                         choose-card
                         0
                         (lambda args #f)
                         (lambda (hi hands)
                           (compute-score hi)))))))))
