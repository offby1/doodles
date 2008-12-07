#lang scheme

(require (planet "set.ss"  ("offby1" "offby1.plt")))

(provide all-neighbors
         with-neato-output
         random-word-pair)

(define *dictionary-file-name*
  (let ((t (get-preference 'anagrams-dictionary-file-name)))
    (or (and t (bytes->path t))
        (string->path "/usr/share/dict/words"))))

(define *words-by-length*
  (let ((w-b-l  (make-hash)))
    (define (note word)
      (let* ((l (string-length word))
             (same-length-words (hash-ref w-b-l l (lambda () (make-set)))))
        (set! same-length-words
              (add same-length-words
                   (string-downcase word)))
        (hash-set! w-b-l l same-length-words)))
    (with-input-from-file *dictionary-file-name*
      (lambda ()
        (let read-word-loop ((word  (read-line)))
          (if (eof-object? word)
              w-b-l
              (begin
                (note word)
                (read-word-loop (read-line)))))))))

(define *the-alphabet*
  (list->vector
   (for/list ([c (in-range (char->integer #\a)
                           (char->integer #\z))])
     (integer->char c))))

(define (25-varieties word index)
  (let 25-varieties-loop ((letters-to-examine (vector-length *the-alphabet*))
                          (result '()))
    (if (zero? letters-to-examine  )
        result
        (let ((this-letter (vector-ref *the-alphabet* (sub1 letters-to-examine))))

          (if (char=? this-letter (string-ref word index))

              ;; don't return the string we were passed in.
              (25-varieties-loop (- letters-to-examine 1)
                                 result)

              (let ((new (string-copy word)))
                (string-set! new index this-letter)
                (25-varieties-loop (- letters-to-examine 1)
                                   (cons new result))))))))

;; one-letter variants
(define (olvs word)
  (let olvs-loop ((letters-to-examine (string-length word))
                  (result '()))
    (if (zero? letters-to-examine)
        result
        (olvs-loop (sub1 letters-to-examine)
                   (append (25-varieties word (sub1 letters-to-examine))
                           result)))))

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


