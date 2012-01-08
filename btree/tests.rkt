#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui)
(require "btree.rkt")

(require/expose
 "btree.rkt"
 (
  decapitate
  list->tree
  public-make-tree
  ql->t
  tree-MaxNodeCount
  tree-count
  tree-empty?
  tree-iterate-first
  tree-iterate-key
  tree-iterate-next
  tree-ref
  tree-remove
  tree-set
  ))

(define (check-round-trip . seq)
  (check-equal? (map car (tree->alist (ql->t seq)))
                (sort seq <)
                (format "Round-tripping ~a" seq)))
(define-test-suite iterate-tests
  (check-false  (tree-iterate-first (public-make-tree)))
  (check-round-trip 2)
  (check-round-trip 2 3 1)
  (check-round-trip 7 0 6 4)
  (check-round-trip 0 1)
  (check-equal? 3 (tree-count (ql->t '(1 2 3))))
  (apply check-round-trip (build-list 100 values)))

(define-test-suite decapitate-tests
  (for ([permutation '((1 2 3)
                       (1 3 2)
                       (3 1 2)
                       (3 2 1)
                       (2 3 1)
                       (2 1 3))])
    (check-equal? (map car (tree->alist (decapitate (ql->t permutation))))
                  (sort (cdr permutation) <)
                  (format "~a" permutation))))
(define-test-suite misc-tests
  (let ([t (public-make-tree)])
    (check-equal? (list->tree (tree->alist t)) t)
    (set! t (tree-set t 2 'two))
    (check-equal? (list->tree (tree->alist t)) t))

  (check-equal? (tree-count (ql->t '(1 2 3))) 3)

  (check-equal? (dict-ref (public-make-tree) 0 (thunk 'not-found))
                'not-found))

(define-test-suite more-misc-tests
  (check-true (tree-empty? (public-make-tree)) "empty")
  (let ([t3  (tree-set (public-make-tree) 3 'three)])
    (check-false (tree-empty? t3) "not empty")
    (check-false (tree-ref t3 6 (thunk #f)) "failure thunk when not found")
    (check-equal? (tree-ref t3 6 'not-found) 'not-found "failure non-thunk when not found")
    (check-equal? (tree-ref t3 3)  'three "found 3")
    (let ([t4 (tree-set t3 4 'four)])
      (check-equal? (tree-ref t4 3 ) 'three "3 still in new tree")
      (check-equal? (tree-ref t4 4) 'four "4 in new tree too")

      (for ([permutation '((1 2 3)
                           (1 3 2)
                           (3 1 2)
                           (3 2 1)
                           (2 3 1)
                           (2 1 3))])
        (for ([elt '(1 2 3)])
          (let ([me  (tree-remove (ql->t permutation) elt)]
                [plt (dict-remove (tree->alist (ql->t permutation)) elt)])
            (check-equal? (tree->alist me)
                          plt
                          (format "~a minus ~a => ~a" permutation elt plt)))))

      (let ([t (tree-remove t4 12345)])
        (check-equal? (tree->alist t)
                      (tree->alist t4))
        (set! t (tree-remove t 3))
        (check-equal? (tree->alist t)
                      '((4 . four))))))
  (let ([t (ql->t '(3))])
    (check-not-false (tree-empty? (dict-remove t 3)))))

(define-test-suite super-serious-delete-tests
  (for ([pass (in-range 10)])
    (let* ([seq (build-list 10 values)]
           [t (ql->t (shuffle seq))])
      (for ([elt (shuffle seq)]
            [expected-length (in-range (length seq) -1)])
        (check-not-false (dict-ref t elt))
        (set! t (dict-remove t elt))
        (check-equal? (dict-count t) expected-length)
        (check-false (dict-ref t elt #f))))))

(define-test-suite position-mismatch-tests
  (let* ([t (ql->t '(1 2))]
         [p (tree-iterate-first t)]
         [n (tree-iterate-next t p)])
    (check-equal? (tree-iterate-key t n) 2)
    (let ([another (ql->t '(1 2))])
      (check-exn exn:fail:contract?
                 (thunk (tree-iterate-next another p))))))

;; These may need adjusting if you change the balancing threshold
;; "alpha".
(define-test-suite maxnodecount-tests
  (let ([t (public-make-tree)])
    (check-equal? (tree-MaxNodeCount t) 0)
    (set! t (tree-set t 2 'two))
    (check-equal? (tree-MaxNodeCount t) 1)
    (set! t  (tree-remove t 2))
    ))

(define-test-suite all-tests
  position-mismatch-tests
  super-serious-delete-tests
  decapitate-tests
  iterate-tests
  maxnodecount-tests
  misc-tests
  more-misc-tests)

(define (tree->alist t)
  (dict-map t cons))

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
