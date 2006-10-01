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



(let* (
       (d10 (make-card 'd 10))
       (d2 (make-card 'd 2))
       (ha (make-card 'h 14))
       (s2  (make-card 's 2))
       (s3 (make-card 's 3))
       (first-annotated-card (car (annotated-cards (make-trick (list s2 d10 ha) 'south)))))

  (test/text-ui
   (test-suite
    "The one and only suite"

    (test-case
     "Annotations 1"
     (check cards= (car first-annotated-card) s2))

    (test-case
     "Annotations 2"
     (check eq? (cdr first-annotated-card) 'south))

    (test-equal? "follows suit 1"
                 s3
                 (choose-card
                  (make-history
                   (list (make-trick (list s2 d10 ha) 'east)))
                  (list (ha:make-hand (list s3 d2)))
                  13))
    (test-case
     "follows suit 2"
     (check eq?
            'd
            (card-suit
             (choose-card
              (make-history
               (list (make-trick (list d10 s2 ha) 'west)))
              (list (ha:make-hand (list s3 d2 (make-card 'd 9))))
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
                  (list (make-trick (list d10 s2 ha) 'north)))
                 (list (list s3 ha))
                 13)))
    (test-exn "Can't remove non-existant card from hand"
              exn:fail:contract?
              (lambda ()
                (define s2 (make-card 's 2))
                (define h (ha:make-hand (list s2)))
                (set! h (ha:remove-card h s2))
                (set! h (ha:remove-card h s2))
                ))
    (test-exn "Can't add card twice to hand"
              exn:fail:contract?
              (lambda ()
                (define s2 (make-card 's 2))
                (define h (ha:make-hand (list s2)))
                (ha:add-card! h s2)
                ))

    (test-exn "mt pukes if too few cards"
              exn:fail:contract?
              (lambda ()
                (mt 'north 'c6 'c9 'c3 )))

    (test-exn "mt detects duplicate cards"
              exn:fail:contract?
              (lambda ()
                (mt 'north 'c6 'c9 'c3 'c3)))

    (test-equal? "Winner 1"
                 'west
                 (winner (mt 'north 'c3 'c6 'c9 'cj)))
    (test-equal? "Winner 2"
                 'south
                 (winner (mt 'north 'c3 'c6 'cj 'c9)))
    (test-equal? "Winner 3"
                 'east
                 (winner (mt 'north 'c6 'c9 'c3 'dj))))))

)
