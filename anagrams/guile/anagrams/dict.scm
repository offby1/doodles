(define-module (dict))

(use-modules (ice-9 rdelim))
(use-modules (bag))
(use-modules (srfi srfi-1))

(define-public dictionaries #f)
(define dictionary-alist '())
(define (new-hash-table) (make-hash-table 10000))

(format #t "Reading dictionary ... ")

(define word-acceptable?
  (let ((vowels (string->list "aeiou")))
    (lambda (word)
      (and 
       ;; it's gotta have a vowel.
       (find (lambda (ch) (member ch vowels))
             (string->list word))
       ;; it can't have weird non-ascii-letter characters.
       (not (find (lambda (ch)
                    (or (not (char-alphabetic? ch))
                        (char<? #\z ch ))) (string->list word)))
       ;; it's gotta be two letters long, unless it's `i' or `a'.
       (or (string=? "i" word)
           (string=? "a" word)
           (< 1 (string-length word)))))))

(with-input-from-file
    (if #t
        "/usr/share/dict/words"
      "/tmp/tiny-dictionary")
  (lambda ()
    (let loop ((word (read-delimited "\n")))
      (if (not (eof-object? word))
          (begin
            (set! word (string-downcase! word))
            (if (word-acceptable? word)
                (let ((b (bag word)))
                  (let ((relevant-dictionary (assoc-ref dictionary-alist (bag-size b))))
                    (if (not relevant-dictionary)
                        (begin
                          (set! relevant-dictionary (new-hash-table))
                          (set! dictionary-alist (assoc-set! dictionary-alist (bag-size b) relevant-dictionary))))
                    (let ((anagrams (hashx-ref  hash-bag assoc-bag relevant-dictionary b '())))
                      (if (not (member word anagrams))
                          (hashx-set! hash-bag assoc-bag relevant-dictionary b (cons word anagrams)))))))
            
            (loop (read-delimited "\n")))
        
        ))))
(format #t "done: ")

;; Convert the alist to an array, for quick access.

(let* ((alist  (sort dictionary-alist (lambda args (apply > (map car args)))))
       (size-of-longest-word (caar alist)))
  (set! dictionaries (make-vector (+ 1 size-of-longest-word)))
  (let loop ((alist alist))
    (if (not (null? alist))
        (let ((this (car alist)))
          (vector-set! dictionaries 
                       (car this)
                       (cdr this))
          (loop (cdr alist))))))

;; Ensure each element of the array really is a hash table.  Some
;; might not be if our dictionary is small, and hence lacks words of a
;; particular length.

(format #t "~a total entries in"
        (let loop ((slots-examined 0)
                   (total-entries 0))
          (if (< slots-examined (vector-length dictionaries))
              (begin
                (if (not (hash-table? (vector-ref dictionaries slots-examined)))
                    (vector-set! dictionaries slots-examined (new-hash-table)))
                (loop (+ slots-examined 1)
                      (+ total-entries (length (hash-map->list (lambda args #f) (vector-ref dictionaries slots-examined))) )))
            total-entries)))

(format #t " ~a~%" dictionaries)
