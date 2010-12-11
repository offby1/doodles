#lang racket
;; Report each unique line from stdin
(set-for-each
 (for/fold ([saw (set)])
     ([line (in-lines)])
     (set-add saw line))
 displayln)
