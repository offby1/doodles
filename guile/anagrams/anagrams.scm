(define-module (anagrams))
(use-modules (ice-9 receive))
(use-modules (bag))
(use-modules (dict))
(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))
(use-modules (wrappers))

(define* (all-anagrams bag  #:optional (exclusions '()))
  (append (easy-anagrams   bag exclusions)
          (harder-anagrams bag exclusions)))

(define* (easy-anagrams bag #:optional (exclusions '()))
  (let ((found (hashx-ref hash-bag assoc-bag (vector-ref dictionaries (bag-size bag))
                          bag)))
    (if found (map list found)
      '())))

(define (multi-hash-fold k-v-p-proc init dict-count)
  "Something like hash-fold, except it works over only those
dictionaries that contain words smaller than DICT-COUNT."
  (fold (lambda (this-hash-table prior-stuff)
          (hash-fold k-v-p-proc prior-stuff this-hash-table))
        init
        (map (lambda (size)
               (vector-ref dictionaries size))
             (iota dict-count))))

(defmacro land (list stuff)
  (let ((l (gensym)))
    `(let ((,l ,list))
       (if (or
            (not ,l)
            (null? ,l))
           prior
         ,stuff))))

(define (combine words anagrams)
  "Given a list of WORDS, and a list of ANAGRAMS, creates a new
list of anagrams, each of which begins with one of the WORDS."
  (append-map (lambda (word)
                (map (lambda (an)
                       (cons word an))
                     anagrams))
              words))

(define (harder-anagrams bag exclusions)
  (let ((verbose? (null? exclusions)))
    (multi-hash-fold
     (lambda (key words prior)
       (land (not (member (car words) exclusions))
             (let ((smaller-bag (subtract-bags bag key)))
               (land smaller-bag
                     (let ((anagrams (all-anagrams smaller-bag exclusions)))
                       (if (null? anagrams)
                           (begin (display ".") (force-output)))
                       (land anagrams
                             (begin
                               (for-each 
                                (lambda (an)
                                  (set! exclusions (apply lset-adjoin `(,string=? ,exclusions ,@an)))
                                  (if verbose? (format #t "bag ~a => One anagram: ~s~%" smaller-bag an))
                                  )
                                anagrams)
                               (let ((rv (append prior (combine words anagrams))))
                                 ;;(format #t "Fold-proc: prior is ~s; dict words: ~s; anagrams of remainders: ~s; returning ~s~%" prior words anagrams rv) (force-output)
                                 rv)
                               )))))))
     '()
     (bag-size bag))))

(with-profiling 
 (all-anagrams
  easy-anagrams
  harder-anagrams
  subtract-bags
  member
  not
  lset-adjoin
  append)
 (begin (display (all-anagrams (bag "hemingway"))) (newline)))
