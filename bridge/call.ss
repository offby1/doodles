#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module call mzscheme
  (require
   (planet "test.ss"    ("schematics" "schemeunit.plt" 1))
   (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1)))

  (provide
   make-bid
   level
   denomination

   ;; mzscheme 299.102 (x86 linux) segfaults when the structure has
   ;; only one field.  (Reported to the mailing list 19 April 2005,
   ;; twice).  So we give the "call" structure a dummy field, and hide
   ;; it like this.
   (rename make-call-workaround make-call)

   pass? double?)

  (define-values (struct:bid make-bid bid? bid-ref bid-set!) 
    (make-struct-type
     'bid                               ;name-symbol          
     #f                                 ;super-struct-type    
     2                                  ;init-field-k         
     0                                  ;auto-field-k         
     #f                                 ;auto-v               
     null                               ;prop-value-list      
     #f                                 ;inspector-or-false   
     #f                                 ;proc-spec            
     '(0 1)                             ;immutable-k-list     
     (lambda (level denom name)         ;guard-proc           
       (unless (and (integer? level)
                    (<= 1 level 7))
         (raise-type-error name "integer in [1,7]" level))
       (case denom
         ((clubs diamonds hearts spades notrump)
          'ok)
         (else
          (raise-type-error name "clubs|diamonds|hearts|spades|notrump" denom)))
       (values level denom)
       ))) 
 
  (define level
    (make-struct-field-accessor bid-ref 0))

  (define denomination
    (make-struct-field-accessor bid-ref 1))

  (define-values (struct:call make-call call? call-ref call-set!) 
    (make-struct-type
     'call                              ;name-symbol
     #f                                 ;super-struct-type
     2                                  ;init-field-k
     0                                  ;auto-field-k
     #f                                 ;auto-v
     null                               ;prop-value-list
     #f                                 ;inspector-or-false
     #f                                 ;proc-spec
     '(0 1)                             ;immutable-k-list
     (lambda (thing dummy name)         ;guard-proc           
       (if (bid? thing)
           (values thing #f)
         (case thing
           ((pass double redouble)
            (values thing #f))
           (else
            (raise-type-error name "bid, pass, double, or redouble" thing)))))))

  (define (make-call-workaround thing)
    (make-call thing 0))
  
  (define get-call
    (make-struct-field-accessor call-ref 0))
  
  (define (pass? thing)
    (and (call? thing)
         (eq? 'pass (get-call thing))))

  (define (double? thing)
    (and (call? thing)
         (eq? 'double (get-call thing))))
  )

