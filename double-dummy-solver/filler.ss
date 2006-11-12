#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module filler mzscheme
(require (only (lib "list.ss") sort)
         (only (lib "1.ss" "srfi") fold)
         (lib "cmdline.ss")
         (lib "trace.ss")

         (lib "assert.ss" "offby1")
         (prefix dds: "dds.ss")
         "deck.ss"
         "fill-out-hands.ss"
         "fys.ss"
         (only "hand.ss"
               make-hand
               mh
               mhs
               ps
               seat
               sorted)
         (only "history.ss"
               history-empty?
               history-latest-trick
               history-length
               hi:trick-complete?
               make-history)
         "run-for-a-while.ss"
         (only "trick.ss"
               rotate-until
               winner/int
               *seats*
               *trump-suit*)
         "zprintf.ss")

;; given a history and partially-known hands, generate a random
;; conforming hand, then figure the best card for the first player.
(define (choose-chard history handset max-lookahead)
  (dds:choose-card history (map sorted (fill-out-hands handset history)) max-lookahead #f))

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
(define *num-hands* (make-parameter 1))
(define *dummy* (make-parameter
                 #f
                 (lambda (d)
                   (unless (memq d *seats*)
                     (raise-mismatch-error
                      '*dummy*
                      (format "wanted one of ~d, not " *seats*)
                      d))
                   d)))

(define (choose-best-card-no-peeking history hands max-lookahead quick-and-dirty?)
  (define counts-by-choice (make-hash-table 'equal))
  (define me  (seat (car hands)))
  (let* ((hands (mask-out hands me (*dummy*) (history-empty? history)))
         (fallback (dds:choose-card history hands 0 #t)))
    (if quick-and-dirty? fallback
      (begin
        (for-each
         (lambda (c)
           (hash-table-put!
            counts-by-choice
            c
            (add1 (hash-table-get counts-by-choice c 0))))
         (map car
              (run-for-a-while
               (lambda ()
                 (choose-chard history
                               hands
                               max-lookahead))
               (*seconds-per-card*)
               (lambda (seconds-remaining)
                 (fprintf (current-error-port) "~a" seconds-remaining)
                 (flush-output (current-error-port)))
               )))
        (newline (current-error-port)) (flush-output (current-error-port))
        (let ((alist (hash-table-map counts-by-choice cons)))
          (if (null? alist)
              (begin
                (printf "Oops -- no results.  Not enough time.  Buy a bigger computer.  Falling back to ~a~%" fallback)
                (flush-output)
                fallback)
            (let ((max (fold (lambda (new so-far)
                               (if (< (cdr so-far)
                                      (cdr new))
                                   new
                                 so-far))
                             (car alist)
                             alist))
                  (num-trials (exact->inexact (apply + (map cdr alist)))))
              (printf "~a plays ~a~%~%" me (car max))
              (flush-output)
              (car max))))))))
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
  (("-n" "--number-of-hands") h
   "Number of hands to play"
   (*num-hands* (string->number h)))
  (("-r" "--rng") six-ints
   "pseudo-random-generator vector (six integers)"
   (let ((inp (open-input-string six-ints)))
     (let loop ((ints '()))
       (let ((datum (read inp)))
         (if (eof-object? datum)
             (current-pseudo-random-generator
              (vector->pseudo-random-generator (list->vector (reverse ints))))
           (loop (cons datum ints))))
       )))
  ))
(printf "rng state: ~s~%" (pseudo-random-generator->vector (current-pseudo-random-generator)))
(parameterize ((*dummy* 'n)
               (*trump-suit* 's)
               (*shaddap* #t))
  (let ((me 's))
    (let loop ((hands-played 0))
      (when (< hands-played (*num-hands*))
        (let ((hands
               (deal (vector->list (fisher-yates-shuffle! (list->vector *deck*)))
                     (map (lambda (s) (make-hand '() s)) *seats*))))
          (printf "I am ~s, dummy is ~a~%The hands are:~%" me (*dummy*)) (flush-output)

          (for-each (lambda (h)
                      (ps h (current-output-port))
                      (newline))
                    hands)

          (newline)

          (printf "Trump suit is ~a~%" (*trump-suit*)) (flush-output)
          (dds:play-loop
           (make-history 'w)
           hands
           choose-best-card-no-peeking
           ;; TODO: find a way to adjust this value so that it's as
           ;; large as possible while still not taking too long.  Thus
           ;; on very fast machines we can look ahead further.
           (*lookahead*)
           (lambda (history hands)
             (when (and
                    (not (history-empty? history))
                    (hi:trick-complete? history))
               (let ((t (history-latest-trick history)))
                 (printf "===== ~a won trick ~a with the ~a~%"
                         (cdr (winner/int t))
                         (history-length history)
                         (car (winner/int t))) (flush-output)))
             #f)

           (lambda (history hands)
             (printf "We're done.~%~a~%" history)))
          (loop (add1 hands-played)))))))
)