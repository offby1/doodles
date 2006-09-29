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
         "bridge.ss"
         "card.ss"
         "trick.ss"
         (all-except "history.ss" whose-turn)
         (lib "trace.ss"))

(print-struct #t)


(let* (
       (d10 (make-card 'diamonds 10))
       (d2 (make-card 'diamonds 2))
       (ha (make-card 'hearts 14))
       (s2  (make-card 'spades 2))
       (s3 (make-card 'spades 3))
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
                  (list (list s3 d2))
                  #t))
    (test-equal?  "follows suit 2"
                  d2
                  (choose-card
                   (make-history
                    (list (make-trick (list d10 s2 ha) 'west)))
                   (list (list s3 d2 (make-card 'diamonds 9)))
                   #t))
    (test-exn "Notices garbage in hand"
              exn:fail:contract?
              (lambda ()
                (choose-card (make-history (list))
                             (list (list 77))
                             #t)))
    (test-exn "Notices card in both history and hand"
              exn:fail:contract?
              (lambda ()
                (choose-card
                 (make-history
                  (list (make-trick (list d10 s2 ha) 'north)))
                 (list (list s3 ha))
                 #t))))))

)

