#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module filler mzscheme
(require (only (lib "list.ss") sort)
         (only (lib "1.ss" "srfi") fold)
         (lib "cmdline.ss")
         (lib "trace.ss")

         (lib "assert.ss" "offby1")
         (prefix dds: "dds.ss")
         "fill-out-hands.ss"
         (only "hand.ss"
               make-hand
               mh
               mhs
               ps
               seat
               sorted)
         (only "history.ss"
               history-empty?
               make-history)
         "run-for-a-while.ss"
         (only "trick.ss"
               rotate-until
               *seats*
               *trump-suit*)
         "zprintf.ss")

;; given a history and partially-known hands, generate a random
;; conforming hand, then figure the best card for the first player.
(define (choose-chard history handset max-lookahead)
  (dds:choose-card history (map sorted (fill-out-hands handset history)) max-lookahead))

(define *test-handset*
  (mhs (c3 c6 c9 cj ca d2 d9 dt h7 hj hq s6 s9)
       (ct d4 dj dk h2 h6 ha s3 s4 s5 s8 sq sk)
       (c2 c8 d5 d7 d8 da h4 h9 ht hk s2 s7 st)
       (c4 c5 c7 cq ck d3 d6 dq h3 h5 h8 sj sa)
       ))

(define (mask-out handset me dummy opening-lead?)
  (check-type 'mask-out (lambda (thing) (memq thing *seats*)) me)
  (check-type 'mask-out (lambda (thing) (memq thing *seats*)) dummy)
  ;; assume we're given a handset where we can see all the cards.

  ;; if me and the dummy are the same, "mask off" (i.e., replace
  ;; with ? hands) the odd-numbered elements -- namely the
  ;; opponents.
  (cond
   (opening-lead?
    (list (list-ref handset 0)
          (make-hand '? (seat (list-ref handset 1)))
          (make-hand '? (seat (list-ref handset 2)))
          (make-hand '? (seat (list-ref handset 3)))))
   ((eq? me dummy)
    (list (list-ref handset 0)
          (make-hand '? (seat (list-ref handset 1)))
          (list-ref handset 2)
          (make-hand '? (seat (list-ref handset 3)))))

   (else
    ;; udderwise, mask off the two that are neither me nor the
    ;; dummy.
    (map (lambda (h)
           (if (or (eq? me (seat h))
                   (eq? dummy (seat h)))
               h
             (make-hand '? (seat h))))
         handset))))

;(trace mask-out)

(random-seed 0)

(define *seconds-per-card* (make-parameter
                            5           ; five seconds seems about
                                        ; right -- since there are 52
                                        ; cards in a game, 260 seconds
                                        ; is 4.3 minutes; add some
                                        ; time for the auction, and
                                        ; you should be in the
                                        ; neighborhood of seven
                                        ; minutes, which is typical
                                        ; for a hand in a duplicate
                                        ; club game
                            ))
(define *lookahead* (make-parameter 2))

(define *dummy* (make-parameter
                 #f
                 (lambda (d)
                   (unless (memq d *seats*)
                     (raise-mismatch-error
                      '*dummy*
                      (format "wanted one of ~d, not " *seats*)
                      d))
                   d)))

(define (choose-best-card-no-peeking history hands max-lookahead)
  (define counts-by-choice (make-hash-table 'equal))
  (let ((hands (mask-out hands (seat (car hands)) (*dummy*) (history-empty? history))))
    (printf "~a sees ~a~%"
            (seat (car hands))
            hands)
    (for-each
     (lambda (c)
       (hash-table-put!
        counts-by-choice
        c
        (add1 (hash-table-get counts-by-choice c 0))))
     (run-for-a-while
      (lambda ()
        (choose-chard history
                      hands
                      max-lookahead))
      (*seconds-per-card*)
      (lambda (seconds-remaining)
        (fprintf (current-error-port) "~a seconds remaining...~%" seconds-remaining)
        (flush-output (current-error-port)))
      ))

    (let ((alist (hash-table-map counts-by-choice cons)))
      (if (null? alist)
          ;; TODO -- come up with some default card, rather than puke.
          (error 'choose-best-card-no-peeking "Oops -- no results.  Not enough time.  Buy a bigger computer.")
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
          (printf "And our choice is: ~a, with ~a~%"
                  (caar max)
                  (/ (cdr max) num-trials))
          (flush-output)
          (caar max))))))
;(trace choose-best-card-no-peeking)
(command-line
 "filler"
 (current-command-line-arguments)
 (once-each

  ;;Seeing octal escapes in Emacs?  Try C-x RET c utf-8 RET M-x make
  ;;And use Lucida Console, if your terminal is running on Windows; on
  ;;*nix, '10x20' from Emacs' font menu also works
  (("-s" "--seconds-per-card") spc
   "How long to think about each card"
   (*seconds-per-card* (string->number spc)))
  (("-l" "--lookahead") ml
   "Maximum number of tricks to look ahead when predicting"
   (*lookahead* (string->number ml)))
  ))

(parameterize ((*dummy* 's))
  (let ((me 'n))
    (printf "I am ~s, dummy is ~a~%" me (*dummy*))

    (printf "The hands are ")
    (for-each (lambda (h)
                (ps h (current-output-port))
                (newline))
              *test-handset*)

    (newline)

    (parameterize ((*trump-suit* 's))

      (printf "Trump suit is ~a~%" (*trump-suit*))(flush-output)
      (parameterize ((*shaddap* #t))
        (dds:play-loop
         (make-history 'e)
         *test-handset*
         choose-best-card-no-peeking
         ;; TODO: find a way to adjust this value so that it's as
         ;; large as possible while still not taking too long.  Thus
         ;; on very fast machines we can look ahead further.
         (*lookahead*)
         (lambda ignored #f)
         (lambda (history hands)
           (printf "We're done.~%~a~%" history))))

      ;; TODO -- as usual, replace "sort the list and then throw away its
      ;; cdr" with "use 'fold' to find the maximum value"
      )))


;; ok, now do the above for a while

;; ok, now take the list of results from that, and see which score
;; comes up the most.  Return it.

)