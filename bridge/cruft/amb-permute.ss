#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

;; use `amb' to return permutations that meet certain criteria, one at a time.
(require (lib "list.ss" "srfi" "1")
         (rename (lib "extra.ss" "swindle") amb amb))

(define (permute l)
  ;; (distribute 3 (list         )) => '()
  ;; (distribute 3 (list 'a      )) => ((3 a    ) (a 3    )                    )
  ;; (distribute 3 (list 'a 'b   )) => ((3 a b  ) (a 3 b  ) (a b 3  )          )
  ;; (distribute 3 (list 'a 'b 'c)) => ((3 a b c) (a 3 b c) (a b 3 c) (a b c 3))
  (define (distribute item l)
    (cond
     ((null? l)
      '())
     ((null? (cdr l))
      (list (list item (car l))
            (list (car l) item)))
     (else
      (cons (cons item l)
            (map (lambda (seq)
                   (cons (car l) seq)) (distribute item (cdr l)))))
     ))

  (cond
   ((null? l)
    '())
   ((null? (cdr l))
    (list l))
   (else
    (append-map (lambda (seq)
                  (distribute (car l) seq))
                (permute (cdr l))))))

#|

> (let ((i  (amb 1 2 3 4 5)))
    (amb-assert (even? i))
    i)
=> 2
> (amb)
=> 4
> (amb)
amb: tree exhausted
> 

=====
> (amb-collect (let ((i  (amb 1 2 3 4 5)))
    (amb-assert (even? i))
    i))
=>
(2 4)
|#

(define-syntax amb-apply
  (syntax-rules ()
    ([_ e]
     (let f ([l e])
       (cond
        [(null? l) (amb)]
        [else (amb (car l) (f (cdr l)))])))))

(amb-collect (let* ((one-permutation (amb-apply (permute (iota 5)))))
               (amb-assert (apply > (take one-permutation 4)))
               one-permutation))
