#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module auction mzscheme
  
  (require "contract.ss"
           "call.ss"
           (lib "list.ss" "srfi" "1")
           (lib "trace.ss"))
  (provide
   (rename my-make-auction make-auction)
   auction?
   auction-add!
   auction-length
   auction-contract
   auction-complete?
   copy-auction)

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
                                  
  (define *seats* '(north east south west))

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
      (raise-type-error 'check-seat "north|south|east|west" dealer))

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
            ;; TODO -- raise-type-error throws exn:fail:contract,
            ;; which (despite the presence of the word "contract" :-)
            ;; seems like the wrong exception.  Perhaps I need a new
            ;; exn:fail:insufficient-bid exception or something.
            (raise-type-error "sufficient bid" thing))))

       ((or (double? thing)
            (redouble? thing))
        (let ((r (a-risk a)))
          (unless (or (and (= 1 r)
                           (double? thing))
                      (and (= 2 r)
                           (redouble? thing)))
            (raise-type-error "appropriate double or redouble" thing)))))
      
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
       ((not       last-non-pass) 'undefined)
       ((bid?      last-non-pass) 1)
       ((double?   last-non-pass) 2)
       ((redouble? last-non-pass) 4)
       (else
        (error "internal error -- expected double or redouble; got" last-non-pass)))))

  ;(trace auction-contract nth-successor)
  )
