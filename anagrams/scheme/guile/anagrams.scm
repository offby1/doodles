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
  (let ((in-bag   (bag string))
        (start-time  (get-internal-real-time)))
    
    (init in-bag)
    (let* ((result (all-anagrams-internal in-bag *dictionary* 0))
           (stop-time (get-internal-real-time))
           )
      (pretty-print result)
      (format (current-error-port)
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
                           "max depth ~a: ~a~%"
                           max-depth-so-far
                           a))
                 ,ans)))

(define max-depth-so-far 0)

(define (all-anagrams-internal bag dict depth)
  (let ((rv '()))
    (if (> depth max-depth-so-far)
        (set! max-depth-so-far depth))
    (let loop ((dict dict))
      (if (null? dict)
          rv
        (let* ((key (caar dict))
               (words (cdar dict))
               (smaller-bag (subtract-bags bag key)))
          (if smaller-bag
              (if (bag-empty? smaller-bag)
                  (let ((combined (map list words)))
                    (maybe-dump combined)
                    (set! rv (append! rv combined)))
                (let ((anagrams (all-anagrams-internal smaller-bag dict (+ 1 depth))))
                  (if (not (null? anagrams))
                      (let ((combined (combine words anagrams)))
                        (maybe-dump combined)
                        (set! rv (append! rv combined)))))))
          (loop (cdr dict)))))))

(define (combine words anagrams)
  "Given a list of WORDS, and a list of ANAGRAMS, creates a new
list of anagrams, each of which begins with one of the WORDS."
  (append-map (lambda (word)
                (map (lambda (an)
                       (cons word an))
                     anagrams))
              words))
