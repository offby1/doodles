(define-module (anagrams))
(use-modules (ice-9 debug)
             (ice-9 pretty-print)
             (srfi srfi-1))

(use-modules (bag)
             (dict)
             (exclusions)
             (assert))

(define-macro (prog1 expr . rest)
  (let ((e (gensym)))
    `(let ((,e ,expr))
       (begin
         ,@rest)
       ,e)))

;; TODO -- perhaps, instead of writing to standard out, we should
;; write to a file whose name is a simplified version of the input
;; string.
(define-public (all-anagrams string)
  (let ((in-bag   (bag string))
        (start-time  (get-internal-real-time)))
    
    (init in-bag)
    (let* ((result (all-anagrams-internal in-bag  (empty-exclusions) #t))
           (stop-time (get-internal-real-time))
           )
      (pretty-print result)
      (format #t
              ";; ~a anagrams of ~s: ~a seconds~%"
              (length result)
              string
              (exact->inexact (/
                               (- stop-time
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

(define (all-anagrams-internal bag exclusions top-level?)
  (define rv '())
  (save-exclusions exclusions
    (dict-for-each
     (lambda (key words)
       (if (not (excluded? key exclusions))
           (let ((smaller-bag (subtract-bags bag key)))
             (if smaller-bag
                 (if (bag-empty? smaller-bag)
                     (begin
                       (add-exclusion! exclusions key)
                       (let ((combined (map list words)))
                         (maybe-dump combined)
                         (set! rv (append! rv combined))))
                   (let ((anagrams (all-anagrams-internal smaller-bag exclusions #f)))
                     (if (not (null? anagrams))
                         (begin
                           (add-exclusion! exclusions key)
                           (let ((combined (combine words anagrams)))
                             (maybe-dump combined)
                             (set! rv (append! rv combined))))))))))))
    rv))

(define (combine words anagrams)
  "Given a list of WORDS, and a list of ANAGRAMS, creates a new
list of anagrams, each of which begins with one of the WORDS."
  (append-map (lambda (word)
                (map (lambda (an)
                       (cons word an))
                     anagrams))
              words))
