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
 (only "card.ss" cards=)
 (only "hand.ss"
       cards
       hand?
       mh
       mhs
       unknown?
       )
 (only "history.ss" make-history)
 (only "trick.ss" *seats*)
 "zprintf.ss"
 (only (lib "1.ss" "srfi")
       every
       iota
       lset-union
       partition
       ))

(define *test-hand*
  (mhs (c3 c6 c9 cj ca d2 d9 dt h7 hj hq s6 s9)
       (ct d4 dj dk h2 h6 ha s3 s4 s5 s8 sq sk)
       (c2 c8 d5 d7 d8 da h4 h9 ht hk s2 s7 st)
       (c4 c5 c7 cq ck d3 d6 dq h3 h5 h8 sj sa)
       ))

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

  ;; partition hands into ? and other
  (let-values (((unks knowns)
                (partition unknown? hands) ))
    ;; let "hidden cards" be: *deck* minus known hands minus cards from history

    ;; somehow distribute those hidden cards among the ? hands.  Make
    ;; sure, if the number of ? hands doesn't evenly divide the number
    ;; of hidden cards, that the odd cards go to the hands that haven't
    ;; yet played to the current trick.
    (printf "unknowns: ~s; knowns: ~s~%" unks knowns)
    *test-hand*
    )
  )

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
       *test-hand*
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
       (make-history 'e))))))

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
               (make-history 'e))))))
)