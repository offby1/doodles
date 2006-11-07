#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module filler mzscheme
(require "run-for-a-while.ss"
         "fill-out-hands.ss"
         "dds.ss"
         (only "hand.ss"
               mh
               sorted)
         (only "history.ss"
               make-history))

;; given a history and partially-known hands, generate a random
;; conforming hand, then figure the best card for the first player.
(define (predict handset history)
  (let ((random-handset (map sorted (fill-out-hands handset history))))
    (values
     (choose-card history random-handset 1)
     random-handset)))

(define *test-handset*
  (list
   (mh n (c3 c6 c9 cj ca d2 d9 dt h7 hj hq s6 s9))
   (mh e ?)
   (mh s ?)
   (mh w ?)))

(parameterize ((current-pseudo-random-generator (make-pseudo-random-generator)))
  (random-seed 0)
  (call-with-values
      (lambda () (predict *test-handset* (make-history 'w)))
      (lambda (score filled-in)
        (printf "~%Hmm: ~s => ~s~%" filled-in score))))

;; ok, now do the above for a while

;; ok, now take the list of results from that, and see which score
;; comes up the most.  Return it.

)