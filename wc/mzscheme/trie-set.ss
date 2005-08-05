#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module set mzscheme
  (provide
   set
   is-present?
   add!
   )
  (require
   (only (lib "1.ss" "srfi") alist-cons))

  (define-struct tree (alist) #f)
  
  (define (words-tree-lookup tree word)
    (cond ((null? word) tree)
          ((assoc (car word) tree) =>
           (lambda (pair)
             (words-tree-lookup
              (cdr pair)
              (cdr word))))
          (else #f)))

  (define (words-tree-add tree word value)
    (cond ((null? word) value)
          ((assoc (car word) tree) =>
           (lambda (pair)
             (set-cdr! pair
                       (words-tree-add
                        (cdr pair) (cdr word) value))
             tree))
          (else
           (cons (cons (car word)
                       (words-tree-add
                        '() (cdr word) value))
                 tree))))

  (define (set . words)
    (let ((rv '()))
      (for-each (lambda (w)
                  (set! rv (words-tree-add rv (string->list w) #t)))
                words)
      rv))

  (define (is-present? word set)
    (eq? #t (words-tree-lookup set (string->list word))))

  (define-syntax add!
    (syntax-rules ()
      ((_ word set)
       (begin
         (set! set (words-tree-add set (string->list word) #t))
         set))))
  
  )
