#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module anagrams
    mzscheme
  (require "dict.scm"
           "bag.scm"
           (only (lib "1.ss"  "srfi") filter take))

  (provide all-anagrams)

  (define *num-to-show* 10000)

  (define (all-anagrams string dict-file-name )
    (let ((in-bag   (bag string)))
      (init in-bag dict-file-name)
      (let ((rv (all-anagrams-internal
                 in-bag
                 *dictionary*
                 0
                 *num-to-show*)))
        (fprintf (current-error-port)
                 "~a (of at most ~a) anagrams of ~s~%"
                 (length rv)
                 *num-to-show*
                 string))))

(define (all-anagrams-internal bag dict level num-to-show)
  (define rv '())
  (define (maybe-print thing)
    (when (and (zero? level)
               (positive? num-to-show))
      (display thing)
      (newline)
      (set! num-to-show (sub1 num-to-show))))

  (if (zero? num-to-show)
      '()
    (let loop ((dict dict))
      (if (null? dict)
          rv
        (let ((key   (caar dict))
              (words (cdar dict)))
          (let ((smaller-bag (subtract-bags bag key)))
            (define (return stuff)
                (maybe-print stuff)
                (set! rv (append! rv stuff))  )
            (when smaller-bag
              (if (bag-empty? smaller-bag)
                  (return (map list words))
                (let ((anagrams (all-anagrams-internal
                                 smaller-bag
                                 (filter (lambda (entry) (subtract-bags smaller-bag (car entry)))
                                         dict)
                                 (add1 level)
                                 num-to-show)))
                  (if (not (null? anagrams))
                      (return (combine words anagrams)))))))
          
          (loop (cdr dict))))))
  )

  (define (combine words anagrams)
    "Given a list of WORDS, and a list of ANAGRAMS, creates a new
list of anagrams, each of which begins with one of the WORDS."
    (apply append (map (lambda (word)
                         (map (lambda (an)
                                (cons word an))
                              anagrams))
                       words)))

  (all-anagrams
   (vector-ref
    (current-command-line-arguments)
    0)
   #;"/usr/share/dict/words"
   "words"
   ))

