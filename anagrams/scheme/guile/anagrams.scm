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
  (let ((in-bag         (bag string))
        (start-time     (get-internal-real-time)))
    
    (init in-bag)
    (let* ((result    (all-anagrams-internal in-bag *dictionary* 0))
           (stop-time (get-internal-real-time)))
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
       (for-each (lambda (a) (format (current-error-port) "~a~%" a))
                 ,ans)))

(define-macro (update! stuff)
  `(begin
     (maybe-dump ,stuff)
     (set! rv (append! rv ,stuff))))

(define (all-anagrams-internal bag dict depth)

  (set! dict (filter (lambda (entry) (bag-acceptable? (car entry) bag))
                     dict))

  (let ((rv '()))
    (let loop ((dict dict))
      (if (null? dict)
          rv
        (let* ((words (cdar dict))
               (smaller-bag (subtract-bags bag (caar dict))))

          (if (bag-empty? smaller-bag)
              (update! (map list words))

            (let ((anagrams (all-anagrams-internal smaller-bag dict (+ 1 depth))))
              (if (not (null? anagrams))
                  (update! (combine words anagrams)))))
                                                       
          (loop (cdr dict)))))))

(define (combine words anagrams)
  "Given a list of WORDS, and a list of ANAGRAMS, creates a new
list of anagrams, each of which begins with one of the WORDS."
  (append-map (lambda (word)
                (map (lambda (an)
                       (cons word an))
                     anagrams))
              words))
