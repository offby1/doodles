(define-module (anagrams))
(use-modules (ice-9 receive)
             (ice-9 debug)
             (srfi srfi-1))

(use-modules (bag)
             (dict)
             (exclusions)
             )

(define-public (all-anagrams string)
  (let ((in-bag   (bag string)))
    (init in-bag)
    (all-anagrams-internal in-bag  (empty-exclusions))))

(define (all-anagrams-internal bag exclusions)
  (save-exclusions exclusions
    (append
     (let ((easy-anagrams (and
                           (not (excluded? bag exclusions))
                           (hash-ref *dict* bag))))
       (if easy-anagrams
           (begin
             (add-exclusion! exclusions bag)
             (map list easy-anagrams))
         '()))
     (let ((harder-anagrams '()))
       (hash-fold
        (lambda (key words prior)
          (if (not (excluded? key exclusions))
              (let ((smaller-bag (subtract-bags bag key)))
                (if smaller-bag
                    (let ((anagrams (all-anagrams-internal smaller-bag exclusions)))
                      (if anagrams
                          (begin
                            (add-exclusion! exclusions key)
                            (set! harder-anagrams (append! harder-anagrams (combine words anagrams)))
                            )))))))
        #f
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
