(module contract mzscheme

  (require "call.ss"
           "exceptions.ss"
           (lib "trace.ss"))
  (provide
   (rename my-make-contract make-contract)
   contract-level
   contract-denomination
   contract-declarer
   contract-risk)
  
  (define-struct contract (level denomination declarer risk) #f)
  (define (my-make-contract level denomination declarer risk)

    ;; try to make a bid from the level and the denomination -- if
    ;; they're bogus, make-bid will throw an exception.
    (let ((test-bid (make-bid level denomination)))
      'ok)
    
    (case declarer
      ((north south east west) 'ok)
      (else (raise-bridge-error 'make-contract "declarer" declarer)))
    (case risk
      ((1 2 4) 'ok)
      (else (raise-bridge-error 'make-contract "risk" risk)))

    (make-contract level denomination declarer risk))

  
  )
