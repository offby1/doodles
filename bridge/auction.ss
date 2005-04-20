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
   auction-add!
   auction-length
   auction-contract
   auction-complete?)

  (print-struct #t)

  (define-values (struct:auction make-auction auction? auction-ref auction-set!) 
    (make-struct-type
     'auction                           ;name-symbol
     #f                                 ;super-struct-type
     2                                  ;init-field-k
     1                                  ;auto-field-k
     #f                                 ;auto-v
     null                               ;prop-value-list
     #f                                 ;inspector-or-false
     #f                                 ;proc-spec
     '()                                ;immutable-k-list
     #f                                 ;guard-proc
     ))

  (define (my-make-auction dealer)
    (when (not (memq dealer '(north south east west)))
      (raise-type-error "nort|south|east|west" dealer))
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
         (let ((last-bid (find bid? (get-guts a))))
           (if (not last-bid)
               'passed-out
             (make-contract
              (level last-bid)
              (denomination last-bid)
              ;; BUGBUG -- do something reasonable here
              'north

              (a-risk a))))))

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
  
  )

