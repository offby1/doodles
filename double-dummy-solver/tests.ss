#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module tests mzscheme
(require (planet "test.ss"     ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss"  ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"     ("schematics" "schemeunit.plt" 2))
         (only (lib "1.ss" "srfi")
               every
               filter
               lset-intersection
               append-map)
         (lib "assert.ss" "offby1")
         "dds.ss"
         "card.ss"
         "trick.ss"
         (prefix ha: "hand.ss")
         (all-except "history.ss" whose-turn)
         (lib "trace.ss"))

(print-struct #t)



(let ((first-annotated-card (car (annotated-cards (mt south s2 dt ha)))))

  (test/text-ui
   (test-suite
    "The one and only suite"

    (test-case
     "Annotations 1"
     (check cards= (car first-annotated-card) (mc s2)))

    (test-case
     "Annotations 2"
     (check eq? (cdr first-annotated-card) 'south))

    (test-case
     "follows suit 1"
     (check cards=
            (mc s3)
            (choose-card
             (make-history
              (list (mt east  s2 dt ha)))
             (list (ha:mh north s3 d2))
             13)))
    (test-case
     "follows suit 2"
     (check eq?
            'd
            (card-suit
             (choose-card
              (make-history
               (list (mt west  dt s2 ha)))
              (ha:mhs (s2 da d8)
                      (sa dk d7)
                      (s3 d2 d9)
                      (sk dq d6))
              13))))
    (test-exn "Notices garbage in hand"
              exn:fail:contract?
              (lambda ()
                (choose-card (make-history 'north)
                             (list (list 77))
                             13)))
    (test-exn "Notices card in both history and hand"
              exn:fail:contract?
              (lambda ()
                (choose-card
                 (make-history
                  (list (mt north  dt s2 ha)))
                 (list (ha:mh s3 ha))
                 13)))
    (test-exn "Can't remove non-existant card from hand"
              exn:fail:contract?
              (lambda ()
                (define h (ha:mh  s2))
                (set! h (ha:remove-card h (mc s2)))
                (set! h (ha:remove-card h (mc s2)))
                ))
    (test-exn "Can't add card twice to hand"
              exn:fail:contract?
              (lambda ()
                (define h (ha:mh  s2))
                (ha:add-card! h (mc s2))
                ))

    (test-exn "mt detects duplicate cards"
              exn:fail:contract?
              (lambda ()
                (mt north c6 c9 c3 c3)))

    (test-equal? "Winner 1"
                 'west
                 (winner (mt north c3 c6 c9 cj)))
    (test-equal? "Winner 2"
                 'south
                 (winner (mt north c3 c6 cj c9)))
    (test-equal? "Winner 3"
                 'east
                 (winner (mt north c6 c9 c3 dj))))))

)
