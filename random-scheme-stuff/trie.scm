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
    (if (zero? (string-length key-string))
        (failure-thunk)
      (let* ((first-char (string-ref key-string 0))
             (rest-string (substring key-string 1))
             (probe (assq first-char (trie-alist t))))
        (if (or (not probe)
                (not (box? (cddr probe))))
            (failure-thunk)
          (if (zero? (string-length rest-string))
              (cddr probe)
            (trie-lookup (car probe)
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
    (when (zero? (string-length key-string))
      (raise-type-error 'trie-add "non-empty string" key-string))
    (let ((first-char  (string-ref key-string 0))
          (rest-string (substring key-string 1)))
      (if (= 1 (string-length key-string))
          (set-trie-alist! t (alist-update (trie-alist t) first-char (cons (new-trie) (box datum))))
        (let ((probe (assq first-char (trie-alist t))))
          (when (not probe)
            (set-trie-alist! t (cons first-char (cons (new-trie)) #f)))
          (trie-add! t rest-string datum))
        )
      t))
  
  (define (trie-remove t key-string)
    (when (zero? (string-length key-string))
      (raise-type-error 'trie-remove "non-empty string" key-string))
    (let ((first-char  (string-ref key-string 0))
          (rest-string (substring key-string 1)))
      (if (= 1 (string-length key-string))
          (set-trie-alist! t (alist-delete first-char (trie-alist t)))
        (let ((probe (assq first-char (trie-alist t))))
          (fprintf (current-error-port) "probe: ~s~n" probe)
          (when probe
            (set! t (trie-remove (cadr probe) rest-string))))))
    t)

  (trace trie-remove)

  (define-syntax trie-remove!
    (syntax-rules ()
      ((_ t key-string)
       (set! t (trie-remove t key-string)))))

  (print-struct #t)
  (test/text-ui
   (make-test-suite
    "everything"
    (make-test-case
     "alist-update"
     (let ((a '()))
       (set! a (alist-update a 'key 'data))
       (assert-equal? a '((key . data)))
       (set! a (alist-update a 'key 'frobotz))
       (assert-equal? a '((key . frobotz)))
       (set! a (alist-update a 'sam 'shepherd))
       (assert-equal? a '((sam . shepherd)
                          (key . frobotz)))))
    (make-test-case
     "yow"
     (let ((t (new-trie)))
       (assert-true (trie? t))
       (assert-equal? (trie-alist t) '())
       (assert-false (trie-lookup t "duh" (lambda () #f)))

       (trie-add! t "a" 'zap)
       (fprintf (current-error-port) "Here it is: ~s~n" t)
       (assert-equal? (trie-alist t) `((#\a . (,(new-trie) . #&zap))))
       (assert-equal? (trie-lookup t "a" (lambda () #f)) #&zap)

       (trie-remove! t "a")
       (assert-false (trie-lookup t "a" (lambda () #f)))
       )))))
