(module anagrams
    mzscheme
  (require "assert.scm"
           "exclusions.scm"
           "dict.scm"
           "bag.scm"
           "ports.scm"
           ;(lib "errortrace.ss" "errortrace")
           (lib "mred.ss" "mred") ;for `yield'
           (lib "defmacro.ss")
           (lib "pretty.ss")
           (prefix srfi-1- (lib "1.ss" "srfi")))
  (provide all-anagrams)
  
  (define-macro (prog1 expr . rest)
    (let ((e (gensym)))
      `(let ((,e ,expr))
         (begin
           ,@rest)
         ,e)))

  (define (all-anagrams string dict-file-name callback)
    (let ((in-bag   (bag string)))
      (init in-bag dict-file-name)
      (fprintf status-port "") ;clears the status window
      (all-anagrams-internal
       in-bag
       (empty-exclusions)
       #t
       callback)))

  (define (all-anagrams-internal bag exclusions top-level? callback)
    (define rv '())
    (save-exclusions exclusions
                     (dict-for-each
                      (lambda (key words)
                        (yield)
                        (if (not (excluded? key exclusions))
                            (let ((smaller-bag (subtract-bags bag key)))
                              (if smaller-bag
                                  (if (bag-empty? smaller-bag)
                                      (begin
                                        (add-exclusion! exclusions key)
                                        (let ((combined (map list words)))
                                          (if top-level? (callback combined))
                                          (set! rv (append! rv combined))))
                                    (let ((anagrams (all-anagrams-internal smaller-bag exclusions #f callback)))
                                      (if (not (null? anagrams))
                                          (begin
                                            (add-exclusion! exclusions key)
                                            (let ((combined (combine words anagrams)))
                                              (if top-level? (callback combined))
                                              (set! rv (append! rv combined))))))))))))
                     rv))

  (define (combine words anagrams)
    "Given a list of WORDS, and a list of ANAGRAMS, creates a new
list of anagrams, each of which begins with one of the WORDS."
    (apply append (map (lambda (word)
                         (map (lambda (an)
                                (cons word an))
                              anagrams))
                       words)))
  )