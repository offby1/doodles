#lang scheme

(require (planet offby1/offby1/set))

(provide all-neighbors
         with-neato-output
         random-word-pair)

(define *dictionary-file-name*
  (let ((t (get-preference 'anagrams-dictionary-file-name)))
    (or (and t (bytes->path t))
        (string->path "/usr/share/dict/words"))))

(define *words-by-length*
  (call-with-input-file *dictionary-file-name*
    (lambda (inp)
      (for/fold ([w-b-l  (make-immutable-hash '())])
          ([word (in-lines inp)])
          (let* ((l (string-length word))
                 (same-length-words (hash-ref w-b-l l (lambda () (make-set)))))
            (hash-set w-b-l l (add same-length-words
                                   (string-downcase word))))))))

(define *the-alphabet*
  (list->vector
   (for/list ([c (in-range (char->integer #\a)
                           (add1 (char->integer #\z)))])
     (integer->char c))))

(define (25-varieties word index)
  (for/fold ([result '()])
      ([this-letter (in-vector *the-alphabet*)])
      (if (char=? this-letter (string-ref word index))
          ;; don't return the string we were passed in.
          result
          (let ((new (string-copy word)))
            (string-set! new index this-letter)
            (cons (string->immutable-string new) result)))))

;; one-letter variants
(define (olvs word)
  (for/fold ([result '()])
      ([letters-to-examine (in-range (string-length word))])
      (append (25-varieties word letters-to-examine)
              result)))

(define *graphis-output-port* (make-parameter #f (lambda (v)
                                                   (when (not (output-port? v))
                                                     (raise-type-error '*graphis-output-port* "output-port?" v))
                                                   v)))

(define (all-neighbors word)
  (let ((rv (filter (lambda (n) (is-present? n  (hash-ref *words-by-length* (string-length word)))) (olvs word))))
    (when (*graphis-output-port*)
      (for-each (lambda (n)
                  (fprintf (*graphis-output-port*) "~s -- ~s;~n" word n))
                rv))
    rv))

(define (random-word-pair wlength)
  (let* ((words (list->vector (hash-map (hash-ref *words-by-length* wlength) (lambda (k v) k))))
         (length (vector-length words)))
    (list (vector-ref words (random length))
          (vector-ref words (random length)))))

(define (with-neato-output thunk)
  (parameterize ((*graphis-output-port*
                  (open-output-file "network.dot" #:exists 'truncate/replace)))
    (fprintf (*graphis-output-port*) "Graph fred{~nsize=\"30,30\"~n")
    (begin0
        (thunk)
      (fprintf (*graphis-output-port*) "}~n")
      (close-output-port (*graphis-output-port*)))))
