#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module call mzscheme
  (require
   (planet "test.ss"    ("schematics" "schemeunit.plt" 1))
   (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
   (lib "list.ss" "srfi" "1")
   (lib "13.ss" "srfi")                 ;string-join
   (lib "31.ss" "srfi")                 ;rec
   (lib "trace.ss")
   "constants.ss"
   "exceptions.ss")

  (provide
   make-bid level denomination bid-to-number
   (rename my-call? call?)
   (rename flexible-make-call make-call)
   (rename my-bid? bid?)
   bid>?
   pass? double? redouble?
   call->string)

  (print-struct #t)

  (define (looks-singular? sym)
    (not (char=? #\s (char-downcase (car (reverse (string->list (symbol->string sym))))))))

  (define (make-plural sym)
    (string->symbol (string-append (symbol->string sym) "s")))
  
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
     (rec (bid-guard level denom name)  ;guard-proc
          (unless (and (integer? level)
                       (<= 1 level 7))
            (raise-bridge-error name "integer in [1,7]" level))
          (cond
           ((memq denom *denominations*) (values level denom))
           ((looks-singular? denom)
            (bid-guard level (make-plural denom) name))
           (else
            (raise-bridge-error
             name
             (string-join (map symbol->string *denominations*) "|")
             denom)))
          )
     ))
 
  (define bid-level
    (make-struct-field-accessor bid-ref 0))

  (define bid-denomination
    (make-struct-field-accessor bid-ref 1))

  (define (level thing)
    (call->bid! thing)
    (bid-level thing))

  (define (denomination thing)
    (call->bid! thing)
    (bid-denomination thing))
  
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
          (cond
           ((pair? thing)
            (make-bid (first thing)
                      (last thing)))
           (else (raise-bridge-error 'make-call "legal call" thing))))))))

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

  (define (denom->integer d)
    (let ((l (or (memq d *denominations*)
                 (memq (make-plural d) *denominations*))))
      (when (not l)
        (raise-bridge-error 'denom->integer *denominations*
                            d))
      (- (length *denominations*)
         (length l))))

  (define (bid-to-number b)
    (+ (* (- (level b) 1)
          5)
       (denom->integer (denomination b))))

  (define-syntax call->bid!
    (syntax-rules ()
      ((_ thing)
       (when (call? thing)
         (set! thing (get-call thing))))))
  
  (define (bid>? b1 b2)
    (call->bid! b1)
    (call->bid! b2)
    (> (bid-to-number b1)
       (bid-to-number b2)))
  (define (my-bid? thing)
    (or (bid? thing)
        (bid? (get-call thing))))
  (define (call->string c)
    (cond
     ((pass? c)
      "p-")
     ((double? c)
      "X ")
     ((redouble? c)
      "XX")
     ((bid? (get-call c))
      (call->bid! c)
      (string-append
       (number->string (level c))
       (string-locale-upcase (substring (symbol->string (denomination c))
                                        0 1))))
     (else
      (raise-type-error 'call->string "call" c))))
  ;(trace level denomination)
  )

