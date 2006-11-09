#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module filler mzscheme
(require (only (lib "list.ss") sort)
         (only (lib "1.ss" "srfi") fold)

         (lib "assert.ss" "offby1")
         (prefix dds: "dds.ss")
         "fill-out-hands.ss"
         (only "hand.ss"
               make-hand
               mh
               mhs
               seat
               sorted)
         (only "history.ss"
               make-history)
         "run-for-a-while.ss"
         (only "trick.ss"
               rotate-until
               *seats*
               *trump-suit*)
         "zprintf.ss")

;; given a history and partially-known hands, generate a random
;; conforming hand, then figure the best card for the first player.
(define (choose-chard handset history)
  (dds:choose-card history (zp "filled: ~a~%" (map sorted (fill-out-hands handset history))) 1))

(define *test-handset*
  (mhs (c3 c6 c9 cj ca d2 d9 dt h7 hj hq s6 s9)
       (ct d4 dj dk h2 h6 ha s3 s4 s5 s8 sq sk)
       (c2 c8 d5 d7 d8 da h4 h9 ht hk s2 s7 st)
       (c4 c5 c7 cq ck d3 d6 dq h3 h5 h8 sj sa)
       ))

(define (mask-out handset me dummy)
  (check-type 'mask-out (lambda (thing) (memq thing *seats*)) me)
  (check-type 'mask-out (lambda (thing) (memq thing *seats*)) dummy)
  ;; assume we're given a handset where we can see all the cards.

  ;; rotate the hands so that "me" is the car.
  (let ((handset (rotate-until handset (lambda (h)
                                         (eq? (seat (car h))

                                              me)))))
    ;; if me and the dummy are the same, "mask off" (i.e., replace
    ;; with ? hands) the odd-numbered elements -- namely the
    ;; opponents.
    (if (eq? me dummy)
        (list (list-ref handset 0)
              (make-hand '? (seat (list-ref handset 1)))
              (list-ref handset 2)
              (make-hand '? (seat (list-ref handset 3))))

      ;; udderwise, mask off the two that are neither me nor the
      ;; dummy.
      (map (lambda (h)
             (if (or (eq? me (seat h))
                     (eq? dummy (seat h)))
                 h
               (make-hand '? (seat h))))
           handset))))

(printf "With me n, and dummy n too: ~s~%" (mask-out *test-handset* 'n 'n))
(printf "With me n, and dummy e    : ~s~%" (mask-out *test-handset* 'n 'e))

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
             (choose-chard (mask-out *test-handset* 'n 's)
                           (make-history 'n)))
           5                            ; five seconds seems about
                                        ; right -- since there are 52
                                        ; cards in a game, 260 seconds
                                        ; is 4.3 minutes; add some
                                        ; time for the auction, and
                                        ; you should be in the
                                        ; neighborhood of seven
                                        ; minutes, which is typical
                                        ; for a hand in a duplicate
                                        ; club game
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