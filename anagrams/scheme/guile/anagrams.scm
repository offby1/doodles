#!/usr/bin/guile \
--debug -ds
!#
(define-module (anagrams))
(use-modules (ice-9 debug)
             (ice-9 pretty-print)
             (srfi srfi-1))

(use-modules (bag)
             (dict)
             (exclusions)
             (assert))

;; TODO -- perhaps, instead of writing to standard out, we should
;; write to a file whose name is a simplified version of the input
;; string.
(define-public (all-anagrams string)
  (let ((in-bag         (bag string)))
    
    (init in-bag)
    (let* ((result    (all-anagrams-internal in-bag *dictionary* 0)))
      
      (pretty-print result)
      
      result)))

(define-macro (maybe-dump ans)
  `(if (zero? depth)
       (for-each (lambda (a) (format #t "~a~%" a))
                 ,ans)))

(define-macro (update! stuff)
  `(begin
     (maybe-dump ,stuff)
     (set! rv (append! rv ,stuff))))

(define (all-anagrams-internal bag dict depth)
  (let ((rv '()))
    (let loop ((dict dict))
      (if (null? dict)
          rv
        (let* ((words (cdar dict))
               (smaller-bag (subtract-bags bag (caar dict))))
          (if smaller-bag
              (if (bag-empty? smaller-bag)
                  (update! (map list words))
                (let ((anagrams (all-anagrams-internal smaller-bag dict (+ 1 depth))))
                  (if (not (null? anagrams))
                      (update! (combine words anagrams))))))
          (loop (cdr dict)))))))

(define (combine words anagrams)
  "Given a list of WORDS, and a list of ANAGRAMS, creates a new
list of anagrams, each of which begins with one of the WORDS."
  (append-map (lambda (word)
                (map (lambda (an)
                       (cons word an))
                     anagrams))
              words))

(pretty-print (command-line))
;;(all-anagrams (cadr (command-line)))
