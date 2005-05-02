(module contract mzscheme

  (require "call.ss"
           "exceptions.ss"
           (lib "trace.ss"))
  (provide
   make-contract contract-level contract-denomination contract-declarer contract-risk)
  
  (define-values (struct:contract make-contract contract? contract-ref contract-set!) 
    (make-struct-type
     'contract                          ;name-symbol
     #f                                 ;super-struct-type
     4                                  ;init-field-k
     0                                  ;auto-field-k
     #f                                 ;auto-v
     null                               ;prop-value-list
     #f                                 ;inspector-or-false
     #f                                 ;proc-spec
     '()                                ;immutable-k-list
     (lambda (level denomination declarer risk name) ;guard-proc

       ;; try to make a bid from the level and the denomination -- if
       ;; they're bogus, make-bid will throw an exception.
       (let ((test-bid (make-bid level denomination)))
         'ok)
       
       (case declarer
         ((north south east west) 'ok)
         (else (raise-bridge-error name "declarer" declarer)))
       (case risk
         ((1 2 4) 'ok)
         (else (raise-bridge-error name "risk" risk)))

       (values level denomination declarer risk))
     
     )
    )

  (define contract-level        (make-struct-field-accessor contract-ref 0 'level))
  (define contract-denomination (make-struct-field-accessor contract-ref 1 'denomination))
  (define contract-declarer     (make-struct-field-accessor contract-ref 2 'declarer))
  (define contract-risk         (make-struct-field-accessor contract-ref 3 'risk))
  )
