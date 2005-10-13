#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module m3 mzscheme
  (require "dfs.ss")
  (require "draw.ss")
  (require (lib "trace.ss"))
  (require (only (lib "compat.ss") sort))
  (require (only (lib "1.ss" "srfi") iota zip filter append-map))
  (define visited-nodes (make-hash-table 'equal))
  (define x-coordinate car)
  (define y-coordinate cdr)
  (define (shuffle-list l)
    (map cdr
         (sort (lambda (a b) (< (car a) (car b)))
               (map (lambda (elt)
                      (cons (random) elt))
                    l))))
  (define *x-max* 9)
  (define *y-max* 9)
  
  (define (goal-node? n)
    #f
                                        ;(and (pair? n) (= (car n ) *x-max*) (= (cdr n)  *y-max*))
    )
  (define (set-visited! n) (hash-table-put! visited-nodes n #t))
  (define (visited? n) (hash-table-get visited-nodes n (lambda () #f)))
  (define (enumerate-neighbors node)
    (printf "enumerate-neighbors: node ~s~%" node)
    (shuffle-list
     (filter (lambda (candidate)
               (and (<= 0 (x-coordinate candidate) *x-max*)
                    (<= 0 (y-coordinate candidate) *y-max*)
                    (= 1 (+ (abs (- (x-coordinate candidate)
                                    (x-coordinate node)))
                            (abs (- (y-coordinate candidate)
                                    (y-coordinate node)))))))
             (map (lambda (offset)
                    (cons (+ (car offset)
                             (car node))
                          (+ (cdr offset)
                             (cdr node))))
                 
                  (append-map 
                   (lambda (n) 
                     (map (lambda (m)
                            (cons n m))
                          (iota 3 -1)))
                   (iota 3 -1))))))
                                        ;(trace enumerate-neighbors)
  (generic-dfs '(0 . 0)
               enumerate-neighbors
               '()
               goal-node?
               set-visited!
               visited?)
  )
