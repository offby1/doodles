(define-module (anagrams))
(use-modules (ice-9 debug)
             (ice-9 pretty-print)
             (srfi srfi-1))

(use-modules (bag)
             (dict)
             (exclusions)
             (assert))

(define-macro (begin0 expr . rest)
  (let ((e (gensym)))
    `(let ((,e ,expr))
       (begin
         ,@rest)
       ,e)))

(define-public (all-anagrams string)
  (let ((in-bag   (bag string))
        (start-time  (get-internal-real-time)))
    (init in-bag)
    (begin0
      (pretty-print
       (all-anagrams-internal in-bag  (empty-exclusions) #t))
      (format #t
              ";; ~s: ~a seconds~%"
              string
              (exact->inexact (/
                               (- (get-internal-real-time)
                                  start-time)
                               internal-time-units-per-second))))))

(define-macro (maybe-dump ans)
  `(begin
     (if top-level?
         (for-each (lambda (a)
                     (write a)
                     (newline))
                   ,ans))
     ,ans))

(define (format-bag b)
  (format #f "[~s => ~s]" b (hash-ref *dict* b)))

(define (all-anagrams-internal bag exclusions top-level?)
  (save-exclusions exclusions
    (append
     (let ((easy-anagrams (and
                           (not (excluded? bag exclusions))
                           (hash-ref *dict* bag))))
       (if easy-anagrams
           (begin
             (add-exclusion! exclusions bag)
             (maybe-dump (map list easy-anagrams)))
         '()
         ))
     (let ((harder-anagrams '()))
       (hash-fold
        (lambda (key words prior)
          (assert (not (null? words)))
          (if (not (excluded? key exclusions))
              (let ((smaller-bag (subtract-bags bag key)))
                (if smaller-bag
                    (let ((anagrams (all-anagrams-internal smaller-bag exclusions #f)))
                      (assert anagrams)
                      (if #f
                          (format #t "~a - ~a excluding ~a yields ~s ... ~%"
                                  (format-bag bag)
                                  (format-bag key)
                                  (map format-bag exclusions)
                                  anagrams))
                      (if (not (null? anagrams))
                          (begin
                            (add-exclusion! exclusions key)
                            (let ((combined (combine words anagrams)))
                              (maybe-dump combined)
                              (set! harder-anagrams (append! harder-anagrams combined)))
                            )))))))
        #f                              ;ignored
        *dict*)
       harder-anagrams)
     )))

(define (combine words anagrams)
  "Given a list of WORDS, and a list of ANAGRAMS, creates a new
list of anagrams, each of which begins with one of the WORDS."
  (append-map (lambda (word)
                (map (lambda (an)
                       (cons word an))
                     anagrams))
              words))
