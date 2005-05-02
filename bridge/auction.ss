#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module auction mzscheme
  
  (print-struct #t)
  
  (require "contract.ss"
           "constants.ss"
           "call.ss"
           "misc.ss"
           "exceptions.ss"
           (lib "13.ss" "srfi")         ;string-join
           (lib "list.ss" "srfi" "1")
           (lib "trace.ss"))
  (provide
   (rename my-make-auction make-auction)
   auction?
   auction-add!
   auction-length
   auction-contract
   auction-complete?
   auction-has-a-double?
   auction-max-levels
   auction->string
   (rename my-auction-ref auction-ref)
   auction-score
   copy-auction
   get-dealer)

  (define-values (struct:auction make-auction auction? auction-ref auction-set!) 
    (make-struct-type
     'auction                           ;name-symbol
     #f                                 ;super-struct-type
     2                                  ;init-field-k
     0                                  ;auto-field-k
     #f                                 ;auto-v
     null                               ;prop-value-list
     #f                                 ;inspector-or-false
     #f                                 ;proc-spec
     '()                                ;immutable-k-list
     #f                                 ;guard-proc
     ))

  ;; this _could_ be dangerous: if we ever expose a function that
  ;; modifies the list structure of the guts, then we'd need to change
  ;; this function to do a deep copy.  As it stands now, though,
  ;; that's not necessary, since no function yet modifies list
  ;; structure.  Good thing I wrote a unit test to check for that.
  (define (copy-auction a)
    (make-auction (get-guts a)
                  (get-dealer a)))

  ;; (nth-successor 'north 0) => 'north
  ;; (nth-successor 'north 1) => 'east
  ;; (nth-successor 'north 102) => 'south
  ;; (nth-successor 'north 203) => 'west
  (define (nth-successor seat n)

    (define (seat->number s)
      (list-index (lambda (x)
                    (eq? x s))
                  *seats*))

    (list-ref *seats* (modulo (+ (seat->number seat) n) (length *seats*))))

  (define (my-make-auction dealer)
    (unless (memq dealer *seats*)
      (raise-bridge-error 'check-seat "north|south|east|west" dealer))

    ;; calls are in reverse order: most recent first.  That's simply
    ;; because it's a tad easier to cons new calls onto the front than
    ;; to append them to the end.
    (make-auction '() dealer))
  
  (define get-guts
    (make-struct-field-accessor auction-ref 0 'guts))
  
  (define set-guts!
    (make-struct-field-mutator  auction-set! 0 'guts))
  
  (define get-dealer
    (make-struct-field-accessor auction-ref 1 'dealer))
  
  (define (auction-length a)
    (length (get-guts a)))
  
  (define (my-auction-ref a K)
    (list-ref (reverse (get-guts a)) K))
  
  (define (auction-add! a thing)
    (when (auction-complete? a)
      (error 'auction-complete))
    
    (unless (call? thing)
      (set! thing (make-call thing)))
    
    (let ((guts (get-guts a)))

      (cond
       ((bid? thing)
        ;; find the most recent bid, if there is one
        (let ((last-bid (find bid? guts)))
          (when (and last-bid
                     (not (bid>? thing last-bid)))
            ;; TODO -- raise-bridge-error throws exn:fail:contract,
            ;; which (despite the presence of the word "contract" :-)
            ;; seems like the wrong exception.  Perhaps I need a new
            ;; exn:fail:insufficient-bid exception or something.
            ;;(write a) (newline)
            (raise-bridge-error 'auction-add! "sufficient bid" thing))))

       ((or (double? thing)
            (redouble? thing))
        (let ((r (a-risk a)))
          (unless (or (and (= 1 r)
                           (double? thing))
                      (and (= 2 r)
                           (redouble? thing)))
            (raise-bridge-error auction-add! "appropriate double or redouble" thing)))))
      
      (set-guts! a (cons thing guts))))

  (define (auction-complete? a)
    (and (< 3 (auction-length a))
         (every pass? (take (get-guts a) 3))))
  
  (define (auction-contract a)
    (and (auction-complete? a)
         (let ((down-from-last-bid  (find-tail bid? (get-guts a))))
           (if (not down-from-last-bid)
               'passed-out
             (let* ((last-bid (car down-from-last-bid))
                    (last-bidders-seat (nth-successor
                                        (get-dealer a)
                                        (- (length down-from-last-bid) 1))
                                       ))
               (make-contract
                (level last-bid)
                (denomination last-bid)
                (let loop ((up-to-last-bid (reverse down-from-last-bid))
                           (seat (get-dealer a)))
                  (let ((b (car up-to-last-bid)))
                    (if (and (bid? b)
                             (or (eq? seat last-bidders-seat)
                                 (eq? seat (nth-successor last-bidders-seat 2)))
                             (eq? (denomination b)
                                  (denomination last-bid)))
                        seat
                      (loop (cdr up-to-last-bid)
                            (nth-successor seat 1)))))
                
                (a-risk a)))))))

  (define (a-risk a)
    (let ((last-non-pass (find (lambda (c)
                                 (not (pass? c)))
                               (get-guts a))))
      (cond
       ((not       last-non-pass) 0)
       ((bid?      last-non-pass) 1)
       ((double?   last-non-pass) 2)
       ((redouble? last-non-pass) 4)
       (else
        (error "internal error -- expected double or redouble; got" last-non-pass)))))

  (define (auction-has-a-double? a)
    (any double? (get-guts a)))

  (define (every-other seq)
    (let loop ((l seq)
               (evens '())
               (odds '()))
      (cond 
       ((null? l)
        (values (reverse evens)
                (reverse odds)))
       ((null? (cdr l))
        (values (reverse (cons (car l) evens))
                (reverse odds)))
       (else
        (loop (cddr l)
              (cons (car l) evens)
              (cons (cadr l) odds))))))
  
  (define (auction-max-levels a)

    ;; the first call always belongs to the dealer, so if the calls
    ;; weren't stored in reverse chronological order, we could simply
    ;; say that the dealer's side is the evens, and the dealer's
    ;; opponents are the odds.  But since the calls are in reverse
    ;; order, that's wrong when there's an even number of them.
    (let-values (((one-side other-side) (every-other (get-guts a))))
      (let ((dealers-side one-side)
            (dealers-opps other-side))
        (when (even? (auction-length a))
          (swap! dealers-side dealers-opps))
        (set! dealers-opps (level-or-zero (find bid? dealers-opps)))
        (set! dealers-side (level-or-zero (find bid? dealers-side)))
        (when (< 4 (auction-length a))
          (unless (or (positive? dealers-opps)
                      (positive? dealers-side))
            (error 'auction-max-levels "Neither side seems to have bid, despite a long auction" )))
        
        (cons dealers-side dealers-opps))))
  
  ;(trace auction-max-levels)

  (define (rotate seq count)
    (if (or (zero? count)
            (null? seq)
            (null? (cdr seq)))
        seq
      (rotate (append (cdr seq)
                      (list (car seq)))
              (sub1 count))))

  (define (string-append-map proc seq)
    (apply string-append (map proc seq)))

  (define (auction->string a)
    (let ((seats (let loop ((seats *seats*))
                   (if (not (eq? (get-dealer a) (car seats)))
                       (loop (rotate seats 1))
                     seats))))
      (string-append
       (string-join
        (map (lambda (s)
             (string-locale-upcase (substring (symbol->string s) 0 1)))
             seats)
      (make-string 5 #\space))
     "\n--------------------\n"
       (string-join
        (map (lambda (chunk)
             (string-join chunk (make-string 4 #\space)))
             (group-by (length *seats*) (map call->string (reverse (get-guts a)))))
     
        "\n"))))

  ;; this sure is easier than doing it right!
  (define (auction-score thing)
    (if thing
        (auction-length thing)
      0))
  )
