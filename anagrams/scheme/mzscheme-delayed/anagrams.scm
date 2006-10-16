#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module anagrams
mzscheme
(require "dict.scm"
         "bag.scm"
         (only (lib "1.ss"  "srfi") filter find take)
         (only (lib "list.ss") sort))

(provide all-anagrams)

(define (all-anagrams string dict-file-name )
  (let ((in-bag   (bag string)))
    (all-anagrams-internal
     in-bag
     (init in-bag dict-file-name)
     0
     )))

(define (all-anagrams-internal bag dict level)

  (let loop ((rv '())
             (dict dict))

    (if (null? dict)
        rv

      (let* ((key   (caar dict))
             (words (cdar dict))
             (smaller-bag (subtract-bags bag key)))

        (loop
         (if smaller-bag
             (let ((new-stuff
                    (if (bag-empty? smaller-bag)
                        (map (lambda (w)
                               (cons (list w)
                                     (lambda ()
                                       '())))
                             words)
                      (list
                       (cons
                        words
                        (lambda ()
                          (all-anagrams-internal
                           smaller-bag
                           (filter (lambda (entry)
                                     (subtract-bags
                                      smaller-bag
                                      (car entry)))
                                   dict)
                           (add1 level)
                           )))))))
               (append new-stuff rv))
           rv)
         (cdr dict))))))

(define (combine words anagrams)
  "Given a list of WORDS, and a list of ANAGRAMS, creates a new
list of anagrams, each of which begins with one of the WORDS."
  (apply append (map (lambda (word)
                       (map (lambda (an)
                              (cons word an))
                            anagrams))
                     words)))

(define (dn x)
  (display x)
  (newline))

(let* ((in (vector-ref
            (current-command-line-arguments)
            0))
       (result (all-anagrams
                 in
                 (find file-exists? '("/usr/share/dict/words"
                                      "/usr/share/dict/american-english"))
                 ))
       (sorted (sort
                result
                (lambda (e1 e2)
                  (> (string-length (caar e1))
                     (string-length (caar e2))))))
       )
  (fprintf (current-error-port)
           "Words that can be made from ~s:~%"
           in)
  (for-each dn sorted)
  (newline)))

