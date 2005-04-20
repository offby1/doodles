#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module call mzscheme
  (require
   (planet "test.ss"    ("schematics" "schemeunit.plt" 1))
   (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
   (lib "list.ss" "srfi" "1"))

  (provide
   make-bid
   level
   denomination
   (rename my-call? call?)
   (rename flexible-make-call make-call)
   pass? double? redouble?)

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
     1                                  ;init-field-k
     0                                  ;auto-field-k
     #f                                 ;auto-v
     null                               ;prop-value-list
     #f                                 ;inspector-or-false
     #f                                 ;proc-spec
     '(0)                               ;immutable-k-list
     (lambda (thing name)               ;guard-proc           
       (case thing
         ((pass double redouble)
          (values thing))
         (else
          (make-bid (first thing)
                    (last thing)))))))

  ;; can be called _either_ with a list like (make-call '(1 clubs))
  ;; _or_ with two separate arguments like (make-call 1 'clubs)
  (define flexible-make-call
    (case-lambda
      [(thing)
       (make-call thing)]
      [(level denom)
       (make-call (list level denom))]))

  (define get-call
    (make-struct-field-accessor call-ref 0))
  
  (define (pass? thing)
    (and (call? thing)
         (eq? 'pass (get-call thing))))

  (define (double? thing)
    (and (call? thing)
         (eq? 'double (get-call thing))))

  (define (redouble? thing)
    (and (call? thing)
         (eq? 'redouble (get-call thing))))

  (define (my-call? thing)
    (or (call? thing)
        (bid? thing)))
  )

