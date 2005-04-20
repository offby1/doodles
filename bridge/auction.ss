#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module auction mzscheme
  
  (require "call.ss")
  (provide
   (rename my-make-auction make-auction)
   auction-add!
   auction-length
   auction-contract)

  (define-values (struct:auction make-auction auction? auction-ref auction-set!) 
    (make-struct-type
     'auction                           ;name-symbol
     #f                                 ;super-struct-type
     2                                  ;init-field-k
     2                                  ;auto-field-k
     #f                                 ;auto-v
     null                               ;prop-value-list
     #f                                 ;inspector-or-false
     #f                                 ;proc-spec
     '(1)                               ;immutable-k-list
     #f))

  (define (my-make-auction)
    (make-auction '() 'dummy))
  
  (define get-guts
    (make-struct-field-accessor auction-ref 0 'guts))
  
  (define set-guts!
    (make-struct-field-mutator  auction-set! 0 'guts))
  
  (define (auction-length a)
    (length (get-guts a)))
  
  (define (auction-add! a thing)
    (unless (call? thing)
      (raise-type-error 'auction "call" thing))
    (set-guts! a (cons thing (get-guts a))))

  (define (auction-contract a)
    #f)
  
  )

