#lang scheme

(require srfi/1
         (planet offby1/offby1/shuffle)
         (planet "multiply.scm" ("offby1" "offby1.plt")))

(for/fold ([hands (make-immutable-hash '())])
    ([h (in-list (circular-list 'north 'east 'south 'west))]
     [c (in-list (shuffle (multiply (list (list 'spade 'heart 'diamond 'club)
                                          (build-list 13 (compose add1 add1))))))])
    (hash-update hands h (lambda (existing)
                           (cons c existing))
                 '()))