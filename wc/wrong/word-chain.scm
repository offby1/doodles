#!/usr/bin/guile -s
!#

(use-modules (ice-9 slib))

(require 'hash-table)
(require 'filter)
(require 'note)

(define hash-size 50001)

(define word-chain  #f)

(define (with-output-appended-to-file filename thunk)
  (let ((original-output (current-output-port))
        (output-port #f))
  (dynamic-wind
      (lambda () 
        (set! output-port (open-file filename "a"))
        (set-current-output-port output-port))
      thunk
      (lambda () 
        (set-current-output-port original-output)
        (close-port output-port) (set! output-port #f)))))

(define bleep
  (if #t
      (let ((counter 0))
        (lambda ()
          (for-each display (list (string-ref "\\|/-" (remainder counter 4)) "\r"))
          (force-output)
          (set! counter (+ 1 counter))))
    (lambda () #f)))

;; prevents "stack overflow" error
(debug-options-interface (list 'stack 0))

(let ()
  (define dictionary    #f)
  (define store!        #f)             ; string => undefined
  (define store-value!  #f)             ; string, value => undefined
  (define make-pending! #f)             ; string => undefined
  (define make-white!   #f)             ; string => undefined
  (define make-black!   #f)             ; string => undefined
  ;; path to target
  ;; dead end
  (define lookup        #f)             ; string => (string . value) or #f
  (define dict-for-each #f)             ; (lambda (word)) => undefined

  (let ()
    (define my-hash-ref  (predicate->hash-asso string-ci=?))
    (define my-hash-set! (hash-associator      string-ci=?))
    (define my-hash-zap! (hash-remover         string-ci=?))
    (define unknowns 0)
    (define inc! (lambda () (set! unknowns (+ 1 unknowns))))
    (define dec! (lambda () 
                   (set! unknowns (- unknowns 1))))
    
    (set! dictionary (let () (set! unknowns 0) (make-hash-table hash-size)))
    (set! store!        (lambda (word) (my-hash-set! dictionary word 'unknown) (inc!)))
    (set! store-value!  (lambda (word value) (my-hash-set! dictionary word value) (dec!)))
    (set! make-pending! (lambda (word) (my-hash-set! dictionary word 'pending) (dec!)))
    (set! make-white!   (lambda (word) (my-hash-set! dictionary word 'white  ) (dec!)))
    (set! make-black!   (lambda (word) (my-hash-set! dictionary word 'black  ) (dec!)))
    (set! lookup        (lambda (word) (my-hash-ref  word dictionary)))

    (set! dict-for-each             (lambda (thunk) (hash-for-each (lambda (k v) (thunk k)) dictionary)))

    (set! word-chain 
          (lambda (start finish)

            (define (find-all-neighbors word)
              (define (generate-neighbor-strings)
                (define (all-variants-of-position position)
                  (define variant-of-one-letter 
                    (let ((ordinal-of-a (char->integer #\a)))
                      (define (letter->number ch)
                        (- (char->integer ch) ordinal-of-a))
                      (define (number->letter n)
                        (integer->char (+ ordinal-of-a n)))
                      (lambda (offset)

                        (let ((original-letter (string-ref word position))
                              (return-value (string-copy word)))
                          (string-set! return-value position 
                                       (number->letter 
                                        (remainder 
                                         (+ (letter->number original-letter) offset) 26)))
                          return-value
                          ))                        )
                    )
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
              ;;(trace generate-neighbor-strings)
              (let ((return-value (filter lookup (generate-neighbor-strings))))
                
                ;; output information for "neato"
                ;; (http://www.research.att.com/sw/tools/graphviz)
                (with-output-appended-to-file "/tmp/neato-me" 
                  (lambda ()
                    (let loop ((tmp return-value))
                      (if (not (null? tmp))
                          (begin
                            (if (string<? word (car tmp))
                                (begin
                                  (display word)
                                  (display " -- ")
                                  (display (car tmp))
                                  (display ";")
                                  (newline)))
                            (loop (cdr tmp)))))))

                return-value))

            (define (internal-word-chain here)
              ;;(for-each display (list (length path-to-start) ":" here #\newline))
              ;;(bleep)
              (let ((lkup (lookup here)))
                (if (not lkup)
                    (error "Couldn't find " here " in dictionary!"))
                (let ((here-value (cdr lkup)))
                  (cond  
                   ((or (list? here-value)
                        (memq here-value (list 'black 'pending)))
                    here-value)
                   ((string-ci=? here finish)
                    (store-value! here (list here))
                    (list here))
                   (else
                    (make-pending! here)
                    (let loop ((neighbors (find-all-neighbors here))
                               (shortest-from-here '()))
                      (if (null? neighbors)
                          (begin
                            (if (pair? shortest-from-here)
                                (store-value! here (cons here shortest-from-here))
                              (make-black! here))
                            (cons here shortest-from-here))
                        (let ((tmp (internal-word-chain (car neighbors))))
                          (loop (cdr neighbors)
                                (if (and (pair? tmp)
                                         (< (length tmp)
                                            (length shortest-from-here)))
                                    tmp
                                  shortest-from-here))))))))))

            ;;(trace find-all-neighbors)
            (if (or (not (string? start))
                    (not (string? finish))
                    (not (= (string-length start)
                            (string-length finish))))
                (error "the two arguments must be words of the same length"))

            ;;(trace internal-word-chain)

            (store! start)
            (store! finish)
          
            (display "Reading dictionary ...")
          
            (note #t)
            (let ((store-from-file (lambda ()
                                     (define length (string-length start))
                                     (let loop ((one-line (read-line))
                                                (words-read 0)
                                                (words-stored 0))
                                       (if (eof-object? one-line )
                                           (for-each display (list "read " words-read ", stored " words-stored " words" #\newline))
                                         (let ((ok? (= (string-length one-line)
                                                       length)))
                                           (note 'r)
                                           (if ok?
                                               (store! one-line))
                                           (loop (read-line)
                                                 (+ 1 words-read)
                                                 (+ (if ok? 1 0) words-stored))))))))
              (display "Snarfing main dictionary") (newline)
              (with-input-from-file
                  ;;"/tmp/x"
                  "/usr/share/dict/words"
                store-from-file)
              (display "Snarfing extra words") (newline)
              (with-input-from-file
                  "/home/offby1/doodles/wc/extras"
                store-from-file))

            (display " done")
            ;;(display dictionary)
            (newline)
          
            (internal-word-chain  start)))))

(begin (word-chain (list-ref (command-line) 1) (list-ref (command-line) 2)))
