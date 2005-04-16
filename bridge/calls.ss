#! /bin/sh
#|
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module calls mzscheme
  (require (lib "compat.ss")            ;for "sort", at least
           (rename (lib "1.ss" "srfi") iota iota)
           (planet "test.ss" ("schematics" "schemeunit.plt" 1))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1)))
  
  (provide all-legal-calls-I-could-make-now)
  
  (define (last l)
    (car (list-tail l (- (length l) 1))))
  
  (define (all-legal-calls-I-could-make-now auction-so-far)
    ;; BUGBUG -- doesn't deal with doubles.
    (let* ((last-call   (last auction-so-far))
           )
      (list-tail *all-bids* (+ 1 (bid->number last-call)))))
  
  
  (define level car)
  (define (denomination b)
    (hash-table-get *denoms* (cadr b)))
  
  (define *denoms* (make-immutable-hash-table '((clubs    . 0)
                                                (diamonds . 1)
                                                (hearts   . 2)
                                                (spades   . 3)
                                                (notrump  . 4))))
  
  (define *levels*  (map (lambda (x) (+ 1 x)) (iota 7)))
  
  ;; excludes doubles and redoubles
  (define *all-bids*
    (apply
     append
     (map
      (lambda (d)
        (map (lambda (l)
               (list d l))
             (map car (sort (lambda pairs
                              (apply < (map cdr pairs)))
                            (hash-table-map *denoms* cons)))))
      *levels*)))
  
  (define (bid->number b)
    (+ (* (hash-table-count *denoms*) (- (level b) 1))
       (denomination b)))
  
  (define (bid> . bids)
    (apply > (map bid->number bids)))
  
  (define file-tests
    (make-test-suite
     "Tests for calls.ss"
     (make-test-case
      "35 bids"
      (assert = 35 (length *all-bids*)))))

  (test/text-ui file-tests)
  )
