#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module call mzscheme
  (require
   (planet "test.ss"    ("schematics" "schemeunit.plt" 1))
   (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
   (lib "list.ss" "srfi" "1")
   (only (lib "13.ss" "srfi") string-join)
   (lib "31.ss" "srfi")                 ;rec

   "constants.ss"
   "exceptions.ss")

  (provide
   (rename my-make-bid make-bid)
   level denomination bid-to-number
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
  
  (define-struct bid (level denomination) #f)
  (define (my-make-bid level denom)
    (unless (and (integer? level)
                 (<= 1 level 7))
      (raise-bridge-error 'make-bid "integer in [1,7]" level))
    (cond
     ((memq denom *denominations*)
      (make-bid level denom))
     ((looks-singular? denom)
      (my-make-bid level (make-plural denom)))
     (else
      (raise-bridge-error
       'make-bid
       (string-join (map symbol->string *denominations*) "|")
       denom))))

  (define (level thing)
    (call->bid! thing)
    (bid-level thing))

  (define (denomination thing)
    (call->bid! thing)
    (bid-denomination thing))
  
  (define-struct call (thing) #f)
  (define (my-make-call thing)
    (case thing
      ((pass double redouble)
       (make-call thing))
      (else
       (cond
        ((pair? thing)
         (make-call
          (my-make-bid (first thing)
                                 (last thing))))
        (else (raise-bridge-error 'make-call "legal call" thing))))))

  ;; can be called _either_ with a list like (make-call '(1 clubs))
  ;; _or_ with two separate arguments like (make-call 1 'clubs)
  (define flexible-make-call
    (case-lambda
      [(thing)
       (my-make-call thing)]
      [(level denom)
       (my-make-call (list level denom))]))

  
  
  (define (pass? thing)
    (and (call? thing)
         (eq? 'pass (call-thing thing))))

  (define (double? thing)
    (and (call? thing)
         (eq? 'double (call-thing thing))))

  (define (redouble? thing)
    (and (call? thing)
         (eq? 'redouble (call-thing thing))))

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
         (set! thing (call-thing thing))))))
  
  (define (bid>? b1 b2)
    (call->bid! b1)
    (call->bid! b2)
    (> (bid-to-number b1)
       (bid-to-number b2)))
  (define (my-bid? thing)
    (or (bid? thing)
        (bid? (call-thing thing))))
  (define (call->string c)
    (cond
     ((pass? c)
      "p-")
     ((double? c)
      "X ")
     ((redouble? c)
      "XX")
     ((bid? (call-thing c))
      (call->bid! c)
      (string-append
       (number->string (level c))
       (string-locale-upcase (substring (symbol->string (denomination c))
                                        0 1))))
     (else
      (raise-type-error 'call->string "call" c))))


  )

