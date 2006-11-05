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
               )
         (only "history.ss"
               make-history))

;; given a history and partially-known hands, generate a random
;; conforming hand, then predict how many tricks each player will win.
(define (predict handset history)
  (let ((random-handset (fill-out-hands handset history)))
    (predict-score (choose-card history random-handset 0)
                   history random-handset 0)))

(define *test-handset*
  (list
   (mh n (c3 c6 c9 cj ca d2 d9 dt h7 hj hq s6 s9))
   (mh e ?)
   (mh s ?)
   (mh w ?)))

(printf "Hmm: ~s~%" (predict *test-handset* (make-history 'e)))

;; ok, now do the above for a while

;; ok, now take the list of results from that, and see which score
;; comes up the most.  Return it.

)