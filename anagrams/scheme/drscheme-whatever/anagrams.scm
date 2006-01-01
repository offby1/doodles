#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module anagrams
    mzscheme
  (require "dict.scm"
           "bag.scm"
           ;(lib "errortrace.ss" "errortrace")
           (lib "defmacro.ss")
           (lib "pretty.ss")
           (lib "trace.ss")
           (prefix srfi-1-  (lib "1.ss"  "srfi")))
  
  (provide all-anagrams)
  
  (define-macro (prog1 expr . rest)
    (let ((e (gensym)))
      `(let ((,e ,expr))
         (begin
           ,@rest)
         ,e)))

  (define (all-anagrams string dict-file-name )
    (let ((in-bag   (bag string)))
      (init in-bag dict-file-name)
      (all-anagrams-internal
                 in-bag
                 *dictionary*
                 #t)))

  (define (all-anagrams-internal bag dict top-level?)
    (define rv '())
    (let loop ((dict dict))
      (if (null? dict)
          rv
        (let ((key   (caar dict))
              (words (cdar dict)))
          (let ((smaller-bag (subtract-bags bag key)))
            (if smaller-bag
                (if (bag-empty? smaller-bag)
                    (begin
                      (let ((combined (map list words)))
                        (set! rv (append! rv combined))))
                  (let ((anagrams (all-anagrams-internal smaller-bag dict #f)))
                    (if (not (null? anagrams))
                        (begin
                          (let ((combined (combine words anagrams)))
                            (set! rv (append! rv combined)))))))))
          
          (loop (cdr dict))))))


  (define (combine words anagrams)
    "Given a list of WORDS, and a list of ANAGRAMS, creates a new
list of anagrams, each of which begins with one of the WORDS."
    (apply append (map (lambda (word)
                         (map (lambda (an)
                                (cons word an))
                              anagrams))
                       words)))

  ;(trace all-anagrams-internal)
  (let* ((input  (vector-ref (current-command-line-arguments) 0))
         (rv (all-anagrams input "/usr/share/dict/words")))
    (fprintf (current-error-port) "~a anagrams of ~s: ~a~%"
             (length rv)
             input
             ;rv
             'placeholder
             ))
  )
