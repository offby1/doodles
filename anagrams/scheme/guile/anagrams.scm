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
    (let* ((result (all-anagrams-internal in-bag  (empty-exclusions) 0))
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
                               internal-time-units-per-second)))
      result)))

(define-macro (maybe-dump ans)
  `(if (zero? depth)
       (for-each (lambda (a)
                   (format #t
                           "max depth ~a; ~a exclusions: ~a~%"
                           max-depth-so-far
                           max-exclusion-length-so-far
                           a))
                 ,ans)))

(define max-depth-so-far 0)
(define max-exclusion-length-so-far 0)
(define (all-anagrams-internal bag exclusions depth)
  (let ((rv '())
        (exclusion-length (length exclusions)))
    (if (> depth max-depth-so-far)
        (set! max-depth-so-far depth))
    (if (> exclusion-length max-exclusion-length-so-far)
        (set! max-exclusion-length-so-far exclusion-length))
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
                     (let ((anagrams (all-anagrams-internal smaller-bag exclusions (+ 1 depth))))
                       (if (not (null? anagrams))
                           (begin
                             (add-exclusion! exclusions key)
                             (let ((combined (combine words anagrams)))
                               (maybe-dump combined)
                               (set! rv (append! rv combined))))))))))))
      rv)))

(define (combine words anagrams)
  "Given a list of WORDS, and a list of ANAGRAMS, creates a new
list of anagrams, each of which begins with one of the WORDS."
  (append-map (lambda (word)
                (map (lambda (an)
                       (cons word an))
                     anagrams))
              words))
