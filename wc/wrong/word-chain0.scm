;; given two words of the same length, such as "giants" and "ravens",
;; see how few "steps" it takes to transform one into the other.  A
;; "step" consists of modifying one letter, such that the result is a
;; real word.

;; inspired by the puzzle on NPR's "Weekend Edition", 27 January 2001

(require 'hash-table)
(require 'note)
(note #t)

(define hash-size 107)
(define inc #f)
(let ()
  (define hits (make-hash-table hash-size))
  (define my-hash-ref (predicate->hash-asso string=?))
  (define my-hash-set! (hash-associator string=?))
  (set! inc (lambda (word)
              (let* ((old (my-hash-ref word hits))
                     (number (if old (cdr old) 0)))
                (if (< 1 number)
                    (for-each display (list word ": " number)))
                (my-hash-set! hits word (+ 1 number))))))

(define word-chain  #f)

(let ()
  (define dictionary    #f)
  (define store!        #f)             ; string => undefined
  (define make-pending! #f)             ; string => undefined
  ;; path to target
  (define make-white!   #f)             ; string => undefined
  ;; dead end
  ;;(define make-black!   #f)             ; string => undefined
  (define erase!        #f)             ; string => undefined
  (define lookup        #f)             ; string => (string . value) or #f
  (define dict-for-each-not-pending #f) ; (lambda (word)) => undefined
  (define dict-for-each-unknown     #f) ; (lambda (word)) => undefined
  (let ()
    (define my-hash-ref  (predicate->hash-asso string=?))
    (define my-hash-set! (hash-associator      string=?))
    (define my-hash-zap! (hash-remover         string=?))
    (set! dictionary (make-hash-table hash-size))
    (set! store!        (lambda (word) (my-hash-set! dictionary word 'unknown)))
    (set! make-pending! (lambda (word) (my-hash-set! dictionary word 'pending)))
    (set! make-white!   (lambda (word) (my-hash-set! dictionary word #t))) ; path to target
    ;;(set! make-black!   (lambda (word) (my-hash-set! dictionary word #f))) ; dead end
    (set! erase!        (lambda (word) (my-hash-zap! dictionary word      )))
    (set! lookup        (lambda (word) (my-hash-ref  word dictionary      )))
    (set! dict-for-each-not-pending (lambda (thunk) (hash-for-each (lambda (k v) (if (not (eq? v 'pending)) (thunk k))) dictionary)))
    (set! dict-for-each-unknown     (lambda (thunk) (hash-for-each (lambda (k v) (if      (eq? v 'unknown)  (thunk k))) dictionary))))

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

          (define (find-nonpending-neighbors word)

            ;;(trace differs-by-one-letter)
            (let ((return-value '()))
              (dict-for-each-not-pending 
               (lambda (candidate)
                 (if (differs-by-one-letter candidate word)
                     (set! return-value (cons candidate return-value)))))
              return-value))
          (define (find-unknown-neighbors word)
            (let ((return-value '()))
              (dict-for-each-unknown
               (lambda (candidate)
                 (if (differs-by-one-letter candidate word)
                     (set! return-value (cons candidate return-value)))))
              return-value))

          (define (internal-word-chain path-so-far)
            (define here (car path-so-far))
            ;;(inc whence)
            (cond  
             ((string=? here finish)
              (begin
                (display path-so-far) (newline)
                #t))
             ((not (lookup here))
              (begin
                (for-each display (list "oops: " here " was erased" #\newline))
                #f))
             (t
              (begin
                (make-pending! here)
                (let ((return-value #f))
                  (let loop ((neighbors (
                                         find-unknown-neighbors
                                         ;;find-nonpending-neighbors 
                                         here)))
                    (if (not (null? neighbors))
                        (let ()
                          (define (safe-cdr x) (and x (cdr x)))
                          ;;(for-each display (list "before: " (safe-cdr (lookup (car neighbors)))))
                          (if (and (lookup (car neighbors))
                                   (internal-word-chain (cons (car neighbors) path-so-far)))
                              (set! return-value #t)

                            ;; put the call to "loop" after the "if"
                            ;; to get *all* the chains; otherwise put
                            ;; it in the "else" part to get just the
                            ;; first one
                            )
                          ;;(for-each display (list "; after: " (safe-cdr (lookup (car neighbors)))))
                          (loop (cdr neighbors)))))

                  (if return-value
                      (make-white! here)
                    (erase! here))
                  return-value)))))

          (if (or (not (string? start))
                  (not (string? finish))
                  (not (= (string-length start)
                          (string-length finish))))
              (error "the two arguments must be words of the same length"))

          ;;(trace find-nonpending-neighbors)
          ;;(trace internal-word-chain)

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
          (if (or
               (not (lookup start))
               (not (lookup finish)))
              (error "The words must both be in the dictionary"))

          (internal-word-chain (list start)))))
