#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module fill-out-hands mzscheme
(require
 (planet "test.ss"     ("schematics" "schemeunit.plt" 2))
 (planet "text-ui.ss"  ("schematics" "schemeunit.plt" 2))
 (planet "util.ss"     ("schematics" "schemeunit.plt" 2))
 (lib "trace.ss")
 (lib "assert.ss" "offby1")
 (only "card.ss" cards=)
 "deck.ss"
 (only "hand.ss"
       cards
       copy
       hand?
       make-hand
       mh
       mhs
       seat
       sort!
       unknown?
       )
 (only "history.ss"
       history-card-set
       make-history
       whose-turn
       )
 (only "trick.ss"
       *seats*
       rotate-until
       seat<
       )
 "zprintf.ss"
 (only (lib "1.ss" "srfi")
       append-map
       every
       iota
       lset-difference
       lset-union
       partition
       )
 (only (lib "list.ss") sort))
(provide fill-out-hands)

(define *test-handset*
  (mhs (c3 c6 c9 cj ca d2 d9 dt h7 hj hq s6 s9)
       (ct d4 dj dk h2 h6 ha s3 s4 s5 s8 sq sk)
       (c2 c8 d5 d7 d8 da h4 h9 ht hk s2 s7 st)
       (c4 c5 c7 cq ck d3 d6 dq h3 h5 h8 sj sa)
       ))

(define (shuffle-list l)
  (map
   cdr
   (sort
    (map (lambda (item)
           (cons (random)
                 item))
         l)
    (lambda (a b)
      (< (car a)
         (car b))))))

;; returns a new set of hands that is like the input set, but with
;; each "unknown" hand replaced by a randomly-generated hand.  The
;; cards of the returned hands, combined with the cards of the
;; history, comprise a complete deck.
(define (fill-out-hands hands history)
  (unless (and (pair? hands)
               (= (length *seats*)
                  (length hands))
               (every hand? hands))
    (raise-type-error 'fill-out-hands (format "list of ~a hands" (length *seats*) ) 0 hands))

  (let ((hands
         (map
          copy
          ;; Rotate the hands so that the next guy to play is at the
          ;; front.  This ensures that, if the number of ? hands
          ;; doesn't evenly divide the number of hidden cards, that
          ;; the odd cards go to the hands that haven't yet played to
          ;; the current trick.
          (rotate-until
           hands
           (lambda (hands)
             (eq? (seat (car hands))
                  (whose-turn history)))))))

    ;; partition hands into ? and other
    (let-values (((unks knowns)
                  (partition unknown? hands) ))

      (if (null? unks)
          hands
        (let ((hidden (lset-difference
                       cards=
                       *deck*
                       (append-map cards knowns)
                       (history-card-set history))))

          ;; distribute those hidden cards among the ? hands.

          (let ((unks
                 (deal (shuffle-list hidden)
                       (map (lambda (h)
                              (if (unknown? h)
                                  (make-hand '() (seat h))
                                h))
                            unks))))

            (rotate-until
             (sort (append unks knowns)
                   (lambda (h1 h2)
                     (seat< (seat h1)
                            (seat h2))))
             (lambda (hands)
               (eq? (seat (car hands))
                    (whose-turn history))))))))))
;;(trace fill-out-hands)
(when #f
  (parameterize ((current-pseudo-random-generator (make-pseudo-random-generator)))
    (random-seed 0)
    (for-each
     (lambda (d)
       (for-each (lambda (h)
                   (sort! h))
                 d)
       (display d)
       (newline))
     (let loop ((deals '()))
       (if (= 10 (length deals))
           deals
         (loop (cons (fill-out-hands
                      (list (mh n ?)
                            (mh e ?)
                            (mh s ?)
                            (mh w ?)
                            )
                      (make-history 'n))
                     deals)))))))

;; (printf "Here are some random numbers, to see that we haven't clobbered the default RNG:~%")
;; (printf "These should be different every time you run this test.~%")
;; (printf "~a~%" (map (lambda ignored (random)) (iota 5)))

(exit-if-failed
 (test/text-ui
  (test-suite
   "The one and only suite"

   (test-equal?
    "52 cards returned"
    52
    (length
     (apply
      lset-union
      cards=
      (map
       cards
       (fill-out-hands
        *test-handset*
        (make-history 'e))))))

   (test-equal?
    "52, part deux"
    52
    (length
     (apply
      lset-union
      cards=
      (map
       cards
       (fill-out-hands
        (list (mh n c3 c6 c9 cj ca d2 d9 dt h7 hj hq s6 s9)
              (mh e ?)
              (mh s ?)
              (mh w ?)
              )
        (make-history 's))))))

   (test-exn "Requires four args"
             exn:fail:contract?
             (lambda ()
               (fill-out-hands
                '(#f #f #f)
                (make-history 'e))))

   (test-exn "Requires four hands"
             exn:fail:contract?
             (lambda ()
               (fill-out-hands
                '(#f #f #f 'sam)
                (make-history 'e)))))))
)