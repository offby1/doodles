#lang scheme

(require srfi/1
         srfi/26
         (planet offby1/offby1/shuffle)
         (planet "multiply.scm" ("offby1" "offby1.plt")))

(for/fold ([hands (make-immutable-hash '())])
    ([h (in-list (circular-list 'north 'east 'south 'west))]
     [c (in-list (shuffle (multiply (list (list 'spade 'heart 'diamond 'club)
                                          (build-list 13 (cut + 2 <>))))))])
    (hash-update hands h (cut cons c <>) '()))
