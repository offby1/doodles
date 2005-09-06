#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#
(module trie mzscheme
  (require (only (lib "1.ss" "srfi") alist-delete alist-delete! alist-cons))
  (require (lib "trace.ss"))
  (require (planet "test.ss"    ("schematics" "schemeunit.plt" 1)))
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1)))
  
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

  ;; empty string: failure
  ;; single-character string: create or replace existing alist entry with empty trie and datum
  ;; otherwise:
  ;;   empty alist: add alist entry for this character, with a new empty trie and empty data; call trie-add on that new trie
  ;;   otherwise: recursive call on existing trie, shorter string, and datum
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
       (set! t (trie-remove t key-string)))))

  (print-struct #t)
  (let ((lookup (lambda (t key-string)
                  (trie-lookup t key-string (lambda () #f)))))
    (test/text-ui
     (let ((t (new-trie))
           (a '()))
     (make-test-suite
      "everything"
      (make-test-case
       "alist-update"
       (set! a (alist-update a 'key 'data))
       (assert-equal? a '((key . data)))
       (set! a (alist-update a 'key 'frobotz))
       (assert-equal? a '((key . frobotz)))
       (set! a (alist-update a 'sam 'shepherd))
       (assert-equal? a '((sam . shepherd)
                          (key . frobotz))))
        
      (make-test-case
       "yow"
       (assert-true (trie? t))
       (assert-equal? (trie-alist t) '())
       (assert-false (lookup t "duh")))

      (make-test-case
       "ugh"
       (trie-add! t "a" 'zap)
       (assert-equal? (trie-alist t) `((#\a . (,(new-trie) . #&zap))))
       (assert-equal? (lookup t "a") 'zap))

      (make-test-case
       "sam"
       (trie-remove! t "a")
       (assert-false (lookup t "a")))

      (make-test-case
       "bob"
       (trie-add! t "a" 'letter-a)
       (assert-equal? (lookup t "a"  ) 'letter-a))

      (make-test-case
       "tim"
       (trie-add! t "abc" 'abc)
       (assert-equal? (lookup t "a"  ) 'letter-a)
       (assert-equal? (lookup t "abc") 'abc))
      )))))
