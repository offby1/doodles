#lang scheme

(require (prefix-in set: (planet "set.ss" ("soegaard" "galore.plt" 4 2)))
         (only-in srfi/67 string-compare)
         scheme/runtime-path)

(provide all-neighbors
         random-word-pair)

(define-runtime-path *dictionary-file-name* (build-path "/usr/share/dict/words"))

(define *words-by-length*
  (call-with-input-file *dictionary-file-name*
    (lambda (inp)
      (for/fold ([w-b-l  (make-immutable-hash '())])
          ([word (in-lines inp)])

          (let* ((l (string-length word))
                 (same-length-words (hash-ref w-b-l l (curry set:make-ordered string-compare))))

            (hash-set w-b-l l (set:insert (string-downcase word)
                                          same-length-words)))))))

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

(define (all-neighbors word)
  (filter (lambda (n) (set:member? n  (hash-ref *words-by-length* (string-length word)))) (olvs word)))

(define (random-word-pair wlength)
  (let* ((words (list->vector (set:elements (hash-ref *words-by-length* wlength))))
         (length (vector-length words)))
    (list (vector-ref words (random length))
          (vector-ref words (random length)))))
