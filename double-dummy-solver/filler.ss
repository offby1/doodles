#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module filler mzscheme
(require (only (lib "list.ss") sort)
         (only (lib "1.ss" "srfi")
               every
               first
               fold
               fourth
               second
               third
               )
         (lib "cmdline.ss")
         (lib "trace.ss")

         (lib "assert.ss" "offby1")
         (only "card.ss"
               *suits*
               card-suit
               )
         (prefix dds: "dds.ss")
         "deck.ss"
         "fill-out-hands.ss"
         "fys.ss"
         "hand.ss"
         (only "history.ss"
               cs2
               history-empty?
               history-latest-trick
               history-length
               hi:trick-complete?
               make-history
               whose-turn)
         "run-for-a-while.ss"
         (all-except "trick.ss" whose-turn)
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
              fallback
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
           (loop (cons datum ints)))))))))
(printf "rng state: ~s~%" (pseudo-random-generator->vector (current-pseudo-random-generator)))
(define (random-choice seq)
  (list-ref seq (random (length seq))))

;; list of hands => (list seat-symbol suit-sym-or-#f integer)
(define (plausible-trump-suit hands)

  ;; (list hand hand) => (seat-sym suit-symbol integer)
  (define (best-suit h1 h2)

    ;; alist alist -> alist
    (define (add-counts c1 c2)
      (let loop ((c1 c1)
                 (c2 c2)
                 (result '()))
        (if (null? c1)
            result
          (loop (cdr c1)
                (cdr c2)
                (let ((p1 (car c1))
                      (p2 (car c2)))
                  (cons (cons (car p1)
                              (+ (cdr p1)
                                 (cdr p2)))
                        result))))))

    (let* ((cbs1 (counts-by-suit h1))
           (cbs2 (counts-by-suit h2))
           (counts-by-suit-both-hands (add-counts cbs1 cbs2))
           (best (fold
                  (lambda (suit-count-pair best-so-far)
                    (if (< (cdr suit-count-pair)
                           (cdr best-so-far))
                        best-so-far
                      suit-count-pair))
                  (car counts-by-suit-both-hands)
                  counts-by-suit-both-hands)))
      (cons (if (< (cdr (assoc (car best) cbs1))
                   (cdr (assoc (car best) cbs2)))
                (seat h2) (seat h1))
            (list (car best)
                  (cdr best)))))

  (let ((our-max  (best-suit
                   (list-ref hands 0)
                   (list-ref hands 2)))
        (their-max  (best-suit
                     (list-ref hands 1)
                     (list-ref hands 3)) ))

    (cond
     ((and (= 7
              (third our-max)
              (third their-max)))
      (list (first our-max)
            #f
            7)) ;; both hands are square?  Then notrump.  TODO: pick
                ;; the strongest hand, not just our-max
     ((< (third our-max) (third their-max))
      their-max)
     (else our-max))))

(define (display-side-by-side l1 l2 padding port)
  (port-count-lines! port)
  (for-each (lambda (s1 s2)
              (display s1 port)
              (let-values (((line col pos) (port-next-location port)))
                (display (make-string (- padding col) #\space) port))
              (display s2 port)
              (newline port))
            l1 l2))
(*shaddap* #t)
(let loop ((hands-played 0))
  (when (< hands-played (*num-hands*))
    (let* ((hands
            (deal (vector->list (fisher-yates-shuffle! (list->vector *deck*)))
                  (map (lambda (s) (make-hand '() s)) *seats*)))
           (pts (plausible-trump-suit hands))
           (me (with-seat-circle (first pts) (lambda (circ) (list-ref circ 1)))))
      (parameterize ((*dummy*  (with-seat-circle me (lambda (circ) (list-ref circ 1))))

                     (*trump-suit*  (second pts)))
        (printf "Trump suit is ~a (of which declarer's side has ~a) ~%" (*trump-suit*) (third pts))
        (printf "I am ~s, dummy is ~a~%The hands are:~%" me (*dummy*))(flush-output)
        (let ((hands (rotate-until hands (lambda (hands)
                                           (eq? (seat (first hands))
                                                (*dummy*))))))
          (for-each (lambda (s)
                      (display (string-append (make-string 30 #\space) s) (current-output-port))
                      (newline (current-output-port)))
                    (->stringlist (first hands)))
          (display-side-by-side (->stringlist (fourth hands))
                                (->stringlist (second hands))
                                60
                                (current-output-port))
          (for-each (lambda (s)
                      (display (string-append (make-string 30 #\space) s) (current-output-port))
                      (newline (current-output-port)))
                    (->stringlist (third hands))))
        (flush-output (current-output-port))
        (dds:play-loop
         (make-history 'w)
         hands
         (lambda (history hands max-lookahead quick-and-dirty?)

           ;; have it tell me what it's thinking when this hand plays
           ;; to this trick, but shaddap otherwise.
           (parameterize ((*shaddap* (not (and (= 2 (history-length history))
                                               (eq? 'e (whose-turn history))))))

             (choose-best-card-no-peeking history hands max-lookahead quick-and-dirty?)))
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
           (printf "We're done.~%~aSuits taken by seat: ~a~%" history (cs2 history))))
        (loop (add1 hands-played))))))

)