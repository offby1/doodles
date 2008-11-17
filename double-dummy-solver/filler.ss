#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

(module filler mzscheme
(require (only rnrs/base-6 assert)
         (only (lib "list.ss") sort)
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

         (only "card.ss"
               *suits*
               card-suit
               )
         (prefix dds: "dds.ss")
         "deck.ss"
         "fill-out-hands.ss"
         (planet "fys.ss" ("offby1" "offby1.plt"))
         "hand.ss"
         (only "history.ss"
               trick-summaries
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
(define (choose-card history handset max-lookahead)
  (dds:choose-card history (map sorted (fill-out-hands handset history)) max-lookahead #f))

(define (mask-out handset me dummy opening-lead?)
  (assert ((lambda (thing) (memq thing *seats*)) me))
  (assert ((lambda (thing) (memq thing *seats*)) dummy))
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

  (let* (
         ;; just for fun -- make e/w always play stupidly.  That way,
         ;; if we find that n/w aren't regularly beating them, we'll
         ;; know that something is terribly wrong.  Conversely, if n/w
         ;; regularly beat them, I haven't been wasting my time :-)
         (stupid? (member me '(e w)))

         (hands (mask-out hands me (*dummy*) (history-empty? history)))
         (fallback (dds:choose-card history hands 0 #t)))
    (p "~a ~aplays ~a~%" me
       (if stupid? "stupidly " "")
       (if (or stupid? quick-and-dirty?) fallback
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
                    (choose-card history
                                 hands
                                 max-lookahead))
                  (*seconds-per-card*)
                  (lambda (seconds-remaining)
                    (fprintf (current-error-port) ".")
                    (flush-output (current-error-port)))
                  )))
           (let ((alist (hash-table-map counts-by-choice cons)))
             (if (null? alist)
                 (begin
                   (fprintf (current-error-port) "!") (flush-output (current-error-port))
                   fallback)
               (let* ((sorted (sort alist (lambda (a b)
                                            (> (cdr a)
                                               (cdr b)))))
                      (best (first sorted)))

                 (when (< 1 (length sorted))
                   (let ((2nd-best (second sorted)))
                     (printf "(Second-best choice was ~a, at ~a%) "
                             (car 2nd-best)
                             (round (* 100.0 (/ (cdr 2nd-best)
                                                (cdr best)))))))
                 (car best)))))))
    ))
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
  (("-r" "--rng") vec
   "pseudo-random-generator vector like #6(1888977131 3014825601 3849035281 163056249 698545751 4293483726)"
   (current-pseudo-random-generator
    (vector->pseudo-random-generator (read (open-input-string vec)))))))
(printf "rng state: ~s~%" (pseudo-random-generator->vector (current-pseudo-random-generator)))

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

    (let ((random-choice (if (zero? (random 2))
                             our-max
                           their-max)))
      (cond
       ((and (= 7
                (third our-max)
                (third their-max)))
        (list (first random-choice)
              #f
              7)) ;; both hands are square?  Then notrump.  TODO: pick
       ;; the strongest hand, not just our-max
       ((< (third our-max) (third their-max))
        their-max)
       ((= (third our-max) (third their-max))
        random-choice)
       (else our-max)))))

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
            (deal (vector->list (fisher-yates-shuffle (list->vector *deck*)))
                  (map (lambda (s) (make-hand '() s)) *seats*)))
           (pts (plausible-trump-suit hands))
           (me (with-seat-circle (first pts) (lambda (circ) (list-ref circ 1)))))
      (parameterize ((*dummy*  (with-seat-circle me (lambda (circ) (list-ref circ 1))))
                     (*trump-suit*  (second pts)))
        (printf "~a~%Trump suit is ~a"
                #\page (*trump-suit*))
        (printf (if (*trump-suit*)
                    " (of which declarer's side has ~a)"
                  "; declarer's longest suit has ~a cards")
                  (third pts))
        (printf "~%~s makes the opening lead; dummy is ~a~%" me (*dummy*))(flush-output)
        (printf "The hands are:~%")
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
         (make-history me)
         hands
         (lambda (history hands max-lookahead quick-and-dirty?)

           ;; have it tell me what it's thinking when this hand plays
           ;; to this trick, but shaddap otherwise.
           (parameterize ((*shaddap* (or #t
                                         (not (and (= 2 (history-length history))
                                                   (eq? 'e (whose-turn history)))))))

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
           (printf "We're done.~%~aSuits taken by seat: ~a~%" history (trick-summaries history))))
        (loop (add1 hands-played))))))

)