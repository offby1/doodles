(module trie mzscheme
  (provide new-trie trie-lookup trie-add! trie-remove! trie?)
  (require (only (lib "1.ss" "srfi") alist-delete))

  ;; a trie is an alist that maps characters to pairs.  Each pair is a
  ;; trie and an optional datum.  A datum is represented by a box:
  ;; whatever's in the box is the data.  If the second thing in the
  ;; pair is not a box, that means that there's no data.
  (define-struct trie (alist) #f)

  (define (new-trie) (make-trie '()))

  (define (trie-lookup t key-string failure-thunk)
    (when (not (trie? t))
      (raise-type-error 'trie-lookup "trie" t))
    (if (zero? (string-length key-string))
        (failure-thunk)
      (let* ((first-char (string-ref key-string 0))
             (rest-string (substring key-string 1))
             (probe (assq first-char (trie-alist t))))
        (if (not probe)
            (failure-thunk)
          (if (zero? (string-length rest-string))
              (if (not (box? (cddr probe)))
                  (failure-thunk)
                (unbox (cddr probe)))
            (trie-lookup (cadr probe)
                         (substring key-string 1)
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

  (define (trie-add! t key-string datum)
    (when (not (trie? t))
      (raise-type-error 'trie-add "trie" t))
    (when (zero? (string-length key-string))
      (raise-type-error 'trie-add "non-empty string" key-string))
    (let ((first-char  (string-ref key-string 0))
          (rest-string (substring key-string 1)))
      (if (= 1 (string-length key-string))
          (set-trie-alist! t (alist-update (trie-alist t) first-char (cons (new-trie) (box datum))))
        (let ((probe (assq first-char (trie-alist t))))
          (when (not probe)
            (set! probe (cons first-char (cons (new-trie)  #f)))
            (set-trie-alist! t (list probe)))
          (trie-add! (cadr probe) rest-string datum))
        )
      t))

  (define (trie-remove t key-string)
    (when (zero? (string-length key-string))
      (raise-type-error 'trie-remove "non-empty string" key-string))
    (when (not (trie? t))
      (raise-type-error 'trie-remove "trie" t))
    (let ((first-char  (string-ref key-string 0))
          (rest-string (substring key-string 1)))
      (if (= 1 (string-length key-string))
          (set-trie-alist! t (alist-delete first-char (trie-alist t)))
        (let ((probe (assq first-char (trie-alist t))))
          (when probe
            (set! t (trie-remove (cadr probe) rest-string))))))
    t)

  (define-syntax trie-remove!
    (syntax-rules ()
      ((_ t key-string)
       (set! t (trie-remove t key-string))))))
