(define-module (anagrams))
(use-modules (ice-9 receive))
(use-modules (bag))
(use-modules (dict))
(use-modules (srfi srfi-1))
(use-modules (wrappers))

(define (all-anagrams bag exclusions)
  (append (easy-anagrams   bag exclusions)
          (harder-anagrams bag exclusions)))

(define (easy-anagrams bag exclusions)
  (let ((found (hash-ref *dict* bag)))
    (if found
        (begin
          (hash-set! exclusions found #t)
          (map list found))
      '())))

(define (combine words anagrams)
  "Given a list of WORDS, and a list of ANAGRAMS, creates a new
list of anagrams, each of which begins with one of the WORDS."
  (append-map (lambda (word)
                (map (lambda (an)
                       (cons word an))
                     anagrams))
              words))

(define (harder-anagrams bag exclusions)
  (let ((rv '()))
    (hash-for-each
     (lambda (key words)
       (if (not (hash-ref exclusions key))
             (let ((smaller-bag (subtract-bags bag key)))
               (if smaller-bag
                   (let ((anagrams (all-anagrams smaller-bag exclusions)))
                     (if (null? anagrams)
                         (begin (display ".") (force-output)))
                     (if anagrams
                         (begin
                           (hash-set! exclusions key #t)
                           (set! rv (append! rv (combine words anagrams)))
                           )))))))
     *dict*)
    rv))

(let ((input (bag "Ernest Hemingway")))
  (init input)
  (let ((output (with-profiling 
                 (
                  all-anagrams
                  append
                  bag
                  easy-anagrams
                  harder-anagrams
                  hash-ref
                  hash-set!
                  not
                  subtract-bags
                  )
   
                 (all-anagrams input (make-hash-table)))))
    (format #t
            "~a => ~a anagrams: ~a~%"
            input
            (length output)
            output)))

