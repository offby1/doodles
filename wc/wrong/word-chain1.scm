#!/usr/bin/guile -s
!#

;; given two words of the same length, such as "giant" and "raven",
;; see how few "steps" it takes to transform one into the other.  A
;; "step" consists of modifying one letter, such that the result is a
;; real word.  e.g.,

;; cat -> hat -> hit -> hot -> pot -> pox etc.

;; inspired by the puzzle on NPR's "Weekend Edition", 27 January 2001

(use-modules (ice-9 slib))

(require 'hash-table)
(require 'filter)
(require 'note)
(note #t)

(define hash-size 50001)

(define word-chain  #f)

(define bleep
  (if #t
      (let ((counter 0))
        (lambda ()
          (for-each display (list (string-ref "\\|/-" (remainder counter 4)) "\r"))
          (force-output)
          (set! counter (+ 1 counter))))
    (lambda () #f)))

(debug-options-interface (list 'stack 0))
(let ()
  (define dictionary    #f)
  (define store!        #f)             ; string => undefined
  (define make-pending! #f)             ; string => undefined
  ;; path to target
  (define make-white!   #f)             ; string => undefined
  ;; dead end
  (define erase!        #f)             ; string => undefined
  (define lookup        #f)             ; string => (string . value) or #f
  (define dict-for-each #f)             ; (lambda (word)) => undefined

  (let ()
    (define my-hash-ref  (predicate->hash-asso string=?))
    (define my-hash-set! (hash-associator      string=?))
    (define my-hash-zap! (hash-remover         string=?))
    (define unknowns 0)
    (define inc! (lambda () (set! unknowns (+ 1 unknowns))))
    (define dec! (lambda () 
                   (set! unknowns (- unknowns 1))))
    
    (set! dictionary (let () (set! unknowns 0) (make-hash-table hash-size)))
    (set! store!        (lambda (word) (my-hash-set! dictionary word 'unknown) (inc!)))
    (set! make-pending! (lambda (word) (my-hash-set! dictionary word 'pending) (dec!)))
    (set! make-white!   (lambda (word) (my-hash-set! dictionary word #t)       (dec!))) ; path to target
    (set! erase!        (lambda (word) (my-hash-zap! dictionary word) (for-each display (list "Erased " word #\newline)) (dec!)))
    (set! lookup        (lambda (word) (my-hash-ref  word dictionary)))

    (set! dict-for-each             (lambda (thunk) (hash-for-each (lambda (k v) (thunk k)) dictionary)))

    (set! word-chain 
          (lambda (start finish)

            (define (differs-by-one-letter a b)
              (define (count-differing-letters)
                (let loop ((letters-to-examine (string-length a))
                           (differences 0))
                  (if (or
                       (zero? letters-to-examine)
                       (< 1 differences))
                      differences
                    (loop (- letters-to-examine 1)
                          (+ differences (if (char=? (string-ref a (- letters-to-examine 1))
                                                     (string-ref b (- letters-to-examine 1)))
                                             0
                                           1))))))
              ;; BUGBUG -- remove this clause once we've pruned the
              ;; dictionary so that all words are the same length
              (and (= (string-length a)
                      (string-length b))
                   (= 1 (count-differing-letters))))

            (define (find-all-neighbors word)
              (let ((return-value '()))
                (dict-for-each
                 (lambda (candidate)
                   (if (differs-by-one-letter candidate word)
                       (set! return-value (cons candidate return-value)))))
                return-value))

            (define (find-all-neighbors2 word)
              (define (generate-neighbor-strings)
                (define (all-variants-of-position position)
                  (define (variant-of-one-letter offset)
                    (let ((ordinal-of-a  (char->integer #\a)))
                      (define (letter->number ch)
                        (- (char->integer ch) ordinal-of-a))
                      (define (number->letter n)
                        (integer->char (+ ordinal-of-a n)))
                      (let ((original-letter (string-ref word position))
                            (return-value (string-copy word)))
                        (string-set! return-value position 
                                     (number->letter 
                                      (remainder 
                                       (+ (letter->number original-letter) offset) 26)))
                        return-value
                        )))
                  (let loop ((chars-tried 0)
                             (return-value '()))
                    (if (= 25 chars-tried)
                        return-value
                      (loop (+ 1 chars-tried)
                            (cons (variant-of-one-letter (+ 1 chars-tried)) return-value)))))

                (let loop ((chars-to-twiddle (string-length word))
                           (return-value '()))
                  (if (zero? chars-to-twiddle)
                      return-value
                    (loop (- chars-to-twiddle 1)
                          (append (all-variants-of-position (- chars-to-twiddle 1))
                                  return-value)))))
              
              (let ((return-value '()))
                (filter lookup (generate-neighbor-strings))))
            
            ;; Preconditions: (car path-so-far) must be in the
            ;; dictionary.
            ;; Postconditions: if (car path-so-far) was pending on
            ;; entry, it will be pending on exit.  Otherwise, it will be
            ;; either white, or erased.  No other word will be modified.

            (define (internal-word-chain path-to-start)
              (define here (car path-to-start))
              ;;(for-each display (list (length path-to-start) ":" here #\newline))
              (let ((lkup (lookup here)))
                (if (not lkup)
                    (error "Couldn't find " here " in dictionary!"))
                (let ((start-color (cdr lkup)))
                  (cond  
                   ((memq start-color '(pending black white))
                    ;;(display start-color) (newline)
                    start-color)
                   ((string=? here finish)
                    (display (reverse path-to-start)) (newline)
                    (make-white! here)
                    ;;(display 'white) (newline)
                    'white)
                   (else
                    (let ((return-value 'black))
                      (make-pending! here)
                      (let loop ((neighbors (find-all-neighbors2 here)))
                        (if (not (null? neighbors))
                            (begin
                              (if (eq? 'white
                                       (internal-word-chain (cons (car
                                                                   neighbors) path-to-start)))
                                  (set! return-value 'white))
                              (loop (cdr neighbors)))))
                      ;;(display return-value) (newline)
                      return-value))))))

            (if (or (not (string? start))
                    (not (string? finish))
                    (not (= (string-length start)
                            (string-length finish))))
                (error "the two arguments must be words of the same length"))

            ;;(trace internal-word-chain)

            (store! start)
            (store! finish)
          
            (display "Reading dictionary ...")
          
            (with-input-from-file
                ;;"/tmp/x"
                "/usr/share/dict/words"
              (lambda ()
                (define length (string-length start))
                (let loop ((one-line (read-line))
                           (words-read 0)
                           (words-stored 0))
                  (if (eof-object? one-line )
                      (for-each display (list "read " words-read ", stored " words-stored " words" #\newline))
                    (let ((proper-length? (= (string-length one-line)
                                             length)))
                      (note 'r)
                      (if proper-length?
                          (store! one-line))
                      (loop (read-line)
                            (+ 1 words-read)
                            (+ (if proper-length? 1 0) words-stored)))))))
          
            (display " done")
            ;;(display dictionary)
            (newline)
          
            (internal-word-chain (list start))))))

(begin
  (word-chain (list-ref (command-line) 1)
              (list-ref (command-line) 2)))
