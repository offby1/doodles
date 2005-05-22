#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(module tree mzscheme
  (print-struct #t)

  ;; Creates trees of possible auctions.
  (provide best-auction-from-prefix)
  (require "auction.ss"
           "call.ss"
           "misc.ss"
           "exceptions.ss"
           (lib "trace.ss")
           (lib "pretty.ss")
           (only (lib "misc.ss" "swindle") list-of)
           (prefix list- (lib "list.ss"))
           (lib "list.ss" "srfi" "1"))

  (define-syntax false-if-exception
    (syntax-rules ()
      ((_ body-forms ...)
       (with-handlers
           ((exn:fail:bridge?
             (lambda (exn) #f)))
         body-forms ...
         #t))))

  (define (would-be-legal? call incomplete-auction)
    (let ((c (copy-auction incomplete-auction)))
      (false-if-exception
       (auction-add! c call))))

  (define (call>? c1 c2)
    (define (call->int c)
      (if (bid? c)
          (bid-to-number c)
        0))
    (> (call->int c1)
       (call->int c2)))

  (define all-legal-calls
    (let ((all-calls-period
           (reverse
            (list-quicksort
             (map make-call (append '(pass double redouble) (list-of (list x y) (x <- (iota 7 1)) (y <- '(clubs diamonds hearts spades notrump)))))
             call>?))))
      (lambda (i)
        (filter (lambda (c)
                  (would-be-legal? c i))
                all-calls-period)
        )))
                                        ;(trace all-legal-calls)

  (define *best-scoring-auction-so-far* #f)

  ;; I'm not certain that this semaphore is necessary.
  (define *the-semaphore* (make-semaphore 1))
  
  (define (consider-one-auction ca)
    (call-with-semaphore
     *the-semaphore*
     (lambda ()
       (when (or (not *best-scoring-auction-so-far*)
                 (> (auction-score ca)
                    (auction-score *best-scoring-auction-so-far*)))
         (set! *best-scoring-auction-so-far*  ca)
         ;;(printf "Best auction so far: ~a~n" (auction->string *best-scoring-auction-so-far*))
         ))))

  (define (some-auctions-with-given-prefix i)
    (define alc (all-legal-calls i))

    ;; this depeonds upon all-legal-calls being sorted in increasing order.
    (define minimum-bid (find bid? alc))

    ;; a heuristic: don't consider auctions which have two doubles,
    ;; since they're kind of rare.
    (define silly-double?
      (lambda (c)
        (and (auction-has-a-double? i)
             (double? c))))

    (define silly-competition?
      (let* ((maxes (auction-max-levels i))
             (my-side (car maxes))
             (opponents (cdr maxes)))
        (when (odd? (auction-length i))
          (swap! my-side opponents))
      
        (lambda (c)
          ;; actual bid, where opponents have bid to the four level
          (and (bid? c)
               (< 3 opponents)))))

    (define crazy-jump?
      (lambda (c)
        ;; a bid whose level is more than two more than the minimum
        ;; possible
        (and (bid? c)
             (< 2 (- (level-or-zero c)
                     (level-or-zero minimum-bid))))
        ))

                                        ;(trace silly-competition?)
    (unless (and (auction? i)
                 (not (auction-complete? i)))
      (raise-type-error 'some-auctions-with-given-prefix "incomplete auction" i))

    (for-each
     (lambda (c)
       (let ((extended (copy-auction i)))
         (auction-add! extended c)
         (if (auction-complete? extended )
             (consider-one-auction extended)
           (some-auctions-with-given-prefix extended))))
     (remove silly-competition? (remove silly-double? alc)))
    )
                                        ;(trace some-auctions-with-given-prefix)
  
  (define (best-auction-from-prefix a)
    (define thread-id (thread (lambda ()
                                (some-auctions-with-given-prefix a))))
    (let ((seconds-to-wait 1))
      (printf "Waiting ~a seconds for auction generator to come up with some auctions ... " seconds-to-wait) (flush-output)
      (sync/timeout seconds-to-wait thread-id)
      (call-with-semaphore
       *the-semaphore*
       (lambda () (kill-thread thread-id)))
      (printf "done~n"))

    *best-scoring-auction-so-far*)
  ;(trace best-auction-from-prefix)
  )