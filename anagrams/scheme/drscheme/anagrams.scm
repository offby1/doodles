(module anagrams
    mzscheme
  (require "assert.scm"
           "exclusions.scm"
           "dict.scm"
           "bag.scm"
           ;(lib "errortrace.ss" "errortrace")
           (lib "mred.ss" "mred") ;for `yield'
           (lib "defmacro.ss")
           (lib "pretty.ss")
           (prefix srfi-1- (lib "1.ss" "srfi")))
  (provide all-anagrams all-anagrams-mit-callback)

  (define-macro (prog1 expr . rest)
    (let ((e (gensym)))
      `(let ((,e ,expr))
         (begin
           ,@rest)
         ,e)))

  ;; TODO -- perhaps, instead of writing to standard out, we should
  ;; write to a file whose name is a simplified version of the input
  ;; string.
  (define (all-anagrams string)
    (let ((result (time
                   (let ((in-bag   (bag string)))
       
                     (init in-bag "/usr/share/dict/words")
                     (all-anagrams-internal
                      in-bag
                      (empty-exclusions) #t)))))
      
      (pretty-print result)
      (printf (format
               ";; ~a anagrams of ~s~%"
               (length result)
               string))
      ;(output-profile-results #t #t)
      ))

  ;; todo -- combine this with the above, passing the callback as a
  ;; parameter with a default value.
  (define (all-anagrams-mit-callback string dict-file-name)
    (let ((in-bag   (bag string)))
      (init in-bag dict-file-name)
      (fprintf (current-error-port) "") ;clears the status window
      (all-anagrams-internal
       in-bag
       (empty-exclusions)
       #t)))

  (define (string-append-with-spaces words)
    (let loop ((result "")
               (words words))
      (cond
       ((null? words)
        result)
       (#t
        (loop (string-append
               result
               (if (zero? (string-length result))
                   ""
                 " ")
               (car words))
              (cdr words))))))

  (define-macro (maybe-dump ans)
    `(if top-level?
         (for-each
          (lambda (words)
            (printf "~a~%" (string-append-with-spaces words)))
          ,ans)
         ))

  (define (all-anagrams-internal bag exclusions top-level?)
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
    (apply append (map (lambda (word)
                         (map (lambda (an)
                                (cons word an))
                              anagrams))
                       words)))
  )