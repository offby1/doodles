(module trie mzscheme
(provide new-trie trie-lookup trie-add! trie-remove! trie?)
(require (only (lib "1.ss" "srfi") alist-delete))

;; a trie is an alist that maps characters to pairs.  Each pair is a
;; trie and an optional datum.  A datum is represented by a box:
;; whatever's in the box is the data.  If the second thing in the
;; pair is not a box, that means that there's no data.
(define-struct trie (alist) #f)

(define (new-trie) (make-trie '()))

;; returns #f if no such entry
(define (get-alist t key-bytes failure-thunk)
  (and (bytes-length key-bytes)
       (let* ((first-char (bytes-ref key-bytes 0))
              (rest-bytes (subbytes key-bytes 1))
              (probe (assq first-char (trie-alist t))))
         (if (not probe)
             (failure-thunk)
           (if (zero? (bytes-length rest-bytes))
               (if (not (box? (cddr probe)))
                   (failure-thunk)
                 (unbox (cddr probe)))
             (trie-lookup (cadr probe)
                          (subbytes key-bytes 1)
                          failure-thunk))))))

(define (trie-lookup t key-bytes failure-thunk)
  (when (not (trie? t))
    (raise-type-error 'trie-lookup "trie" t))
  (if (zero? (bytes-length key-bytes))
      (failure-thunk)
    (let* ((first-char (bytes-ref key-bytes 0))
           (rest-bytes (subbytes key-bytes 1))
           (probe (assq first-char (trie-alist t))))
      (if (not probe)
          (failure-thunk)
        (if (zero? (bytes-length rest-bytes))
            (if (not (box? (cddr probe)))
                (failure-thunk)
              (unbox (cddr probe)))
          (trie-lookup (cadr probe)
                       (subbytes key-bytes 1)
                       failure-thunk))))))

(define (alist-update alist key data)
  (if (null? alist)
      (list (cons key data))
    (let ((probe (assq key alist)))
      (if probe
          (begin
            (set-cdr! probe data)
            alist)
        (cons (cons key data)
              alist)))))

(define (trie-add! t key-bytes datum)
  (when (not (trie? t))
    (raise-type-error 'trie-add "trie" t))
  (when (zero? (bytes-length key-bytes))
    (raise-type-error 'trie-add "non-empty bytes" key-bytes))
  (let ((first-char  (bytes-ref key-bytes 0))
        (rest-bytes (subbytes key-bytes 1)))
    (if (= 1 (bytes-length key-bytes))
        (set-trie-alist! t (alist-update (trie-alist t) first-char (cons (new-trie) (box datum))))
      (let ((probe (assq first-char (trie-alist t))))
        (when (not probe)
          (set! probe (cons first-char (cons (new-trie)  #f)))
          (set-trie-alist! t (cons  probe (trie-alist t))))
        (trie-add! (cadr probe) rest-bytes datum))
      )
    t))

(define (trie-remove t key-bytes)
  (when (zero? (bytes-length key-bytes))
    (raise-type-error 'trie-remove "non-empty bytes" key-bytes))
  (when (not (trie? t))
    (raise-type-error 'trie-remove "trie" t))
  (let ((first-char  (bytes-ref key-bytes 0))
        (rest-bytes (subbytes key-bytes 1)))
    (if (= 1 (bytes-length key-bytes))
        (set-trie-alist! t (alist-delete first-char (trie-alist t)))
      (let ((probe (assq first-char (trie-alist t))))
        (when probe
          (set! t (trie-remove (cadr probe) rest-bytes))))))
  t)

(define-syntax trie-remove!
  (syntax-rules ()
    ((_ t key-bytes)
     (set! t (trie-remove t key-bytes))))))
