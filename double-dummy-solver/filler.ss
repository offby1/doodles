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
         (only (lib "1.ss" "srfi") fold)
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
  (dds:choose-card history (zp "filled: ~a~%" (map sorted (fill-out-hands handset history))) 0))

(define *test-handset*
  (list
   (mh n c9 cj ca d2 d3 d6 d9 dt h7 hj hq s6 s9)
   (mh e ?)
   (mh s ?)
   (mh w ?)))

(for-each
 (lambda (trumps)
   (parameterize ((*trump-suit* trumps))
     (define counts-by-choice (make-hash-table 'equal))
     (printf "Trump suit is ~a~%" (*trump-suit*))(flush-output)
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
           5
           (lambda (seconds-remaining)
             (fprintf (current-error-port) "~a seconds remaining...~%" seconds-remaining)
             (flush-output (current-error-port)))
           ))))
     ;; TODO -- as usual, replace "sort the list and then throw away its
     ;; cdr" with "use 'fold' to find the maximum value"
     (let ((alist (hash-table-map counts-by-choice cons)))
       (if (null? alist)
           (printf "Oops -- no results.  Not enough time.  Buy a bigger computer.~%")
         (let ((max (fold (lambda (new so-far)
                            (if (< (cdr so-far)
                                   (cdr new))
                                new
                              so-far))
                          (car alist)
                          alist))
               (num-trials (exact->inexact (apply + (map cdr alist)))))
           (printf
            "Counts by choice: ~s~%"
            (sort alist
                  (lambda (a b)
                    (> (cdr a)
                       (cdr b))))) (flush-output)
           (when (not (zero? (length alist)))
             (printf "And the winner is: ~a, with ~a~%"
                     (caar max)
                     (/ (cdr max) num-trials))
             (flush-output)
             )
           )))))
 (list #f 's 'h 'd 'c)
 )


;; ok, now do the above for a while

;; ok, now take the list of results from that, and see which score
;; comes up the most.  Return it.

)