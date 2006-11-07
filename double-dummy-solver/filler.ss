#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module filler mzscheme
(require "run-for-a-while.ss"
         "fill-out-hands.ss"
         (prefix dds: "dds.ss")
         "zprintf.ss"
         (only (lib "list.ss") sort)
         (only "hand.ss"
               mh
               sorted)
         (only "history.ss"
               make-history)
         (only "trick.ss"
               *trump-suit*))

;; given a history and partially-known hands, generate a random
;; conforming hand, then figure the best card for the first player.
(define (choose-chard handset history)
  (dds:choose-card history (fill-out-hands handset history) 1))

(define *test-handset*
  (list
   (mh n c3 c6 c9 cj ca d2 d9 dt h7 hj hq s6 s9)
   (mh e ?)
   (mh s ?)
   (mh w ?)))

(for-each
 (lambda (trumps)
   (parameterize ((*trump-suit* trumps))
     (define counts-by-choice (make-hash-table 'equal))
     (printf "Trump suit is ~a~%" (*trump-suit*))
     (parameterize ((*shaddap* #t))
       (for-each
        (lambda (c)
          (hash-table-put!
           counts-by-choice
           c
           (add1 (hash-table-get counts-by-choice c 0))))
        (parameterize ((current-pseudo-random-generator (make-pseudo-random-generator)))
          (random-seed 0)
          (run-for-a-while
           (lambda ()
             (choose-chard *test-handset* (make-history 'n)))
           20
           (lambda (seconds-remaining) (fprintf (current-error-port) "~a seconds remaining...~%" seconds-remaining))
           ))))
     ;; TODO -- as usual, replace "sort the list and then throw away its
     ;; cdr" with "use 'fold' to find the maximum value"
     (let* ((count-choice-alist (sort (hash-table-map counts-by-choice cons)
                                      (lambda (a b)
                                        (> (cdr a)
                                           (cdr b)))))
            (num-trials (exact->inexact (apply + (map cdr count-choice-alist)))))
       ;;(printf "Counts by choice: ~s~%" count-choice-alist )
       (when (not (zero? (length count-choice-alist)))
         (printf "And the winner is: ~a, with ~a~%"
                 (car (list-ref count-choice-alist 0))
                 (/ (cdr (list-ref count-choice-alist 0)) num-trials))
         (when (not (zero? (length (cdr count-choice-alist))))
           (printf "(second place is ~a with ~a)~%"
                   (car (list-ref count-choice-alist 1))
                   (/ (cdr (list-ref count-choice-alist 1)) num-trials))))
       )))
 (list #f 'h)
 )


;; ok, now do the above for a while

;; ok, now take the list of results from that, and see which score
;; comes up the most.  Return it.

)