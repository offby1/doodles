#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

#|
> (unfold 
   (lambda (i) (< 10 i))                ;p
   (lambda (x) x)                       ;f
   add1                                 ;g
   0                                    ;seed
   )
(0 1 2 3 4 5 6 7 8 9 10)
> |#
(require (lib "trace.ss")
         (planet "test.ss" ("schematics" "schemeunit.plt" 1))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1)))

;; see SICP, page 321
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b)
     (cons a (delay  b)))))

(define stream-car car)
(define stream-cdr (lambda (p) (force (cdr p))))
(define the-null-stream '())
(define stream-null? null?)

(define (stream->list stream)
  (if (or (stream-null? stream)
          (stream-null? (cdr stream)))
      stream
    (cons (stream-car stream)
          (stream->list (stream-cdr stream)))))

(define (lazy-map proc seq)
    (if (stream-null? seq)
        the-null-stream
      (cons-stream (proc (car seq))
                   (lazy-map proc (stream-cdr seq)))))

(define (slit . data)
  (let loop ((data (reverse data))
             (result the-null-stream))
    (if (null? data)
        result
      (loop (cdr data)
            (cons-stream (car data) result)))))

(define lazy-unfold
  (let ()
    (define (something p f g seed tail-gen)
      (if (p seed)
          (tail-gen seed)
        (cons-stream (f seed)
                   (lazy-unfold p f g (g seed)))))
    (case-lambda
      [(p f g seed)
       (something p f g seed (lambda (x) the-null-stream))]
      [(p f g seed tail-gen)
       (something p f g seed tail-gen)])))
(trace lazy-unfold)

#|
(define l (lazy-unfold 
     (lambda (i) (< 10 i))              ;p
     (lambda (x) x)                     ;f
     add1                               ;g
     0                                  ;seed
     ))
> l
(0 . #<struct:promise>)
> (stream-cdr l)
(1 . #<struct:promise>)
> (stream-cdr (stream-cdr l))
(2 . #<struct:promise>)
>
|#

;; I _think_ this is the lazy equivalent of "reduce", but I'm not
;; certain.
(define (lazy-reduce combiner-proc init seq)
  (let loop ((seq seq)
             (result init))
    (if (stream-null? seq)
        result
      (loop (force (cdr seq))
            (combiner-proc (car seq) result)))))

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

;; ()         => ()
;; (fred)     => ((fred)     . #<promise -> ()>)
;; (fred sam) => ((fred sam) . #<promise -> ((sam fred) . #<promise -> ()>)>)
(define (lazy-permute l)
  (cond
   ((stream-null? l)
    the-null-stream)
   ((stream-null? (cdr l))
    (slit l))
   (else
    (lazy-map (lambda (seq)
                (distribute (car l) seq))
              (lazy-permute (cdr l))))))

(define (append2 s1 s2)
  (let loop ((s1 s1)
             (s2 s2)
             (result '()))
    (cond
     ((and (null? s1)
           (null? s2))
      (reverse result))
     ((null? s1)
      (loop (cdr s2)
            '()
            (cons (car s2) result)))
     (else
      (loop (cdr s1)
            s2
            (cons (car s1) result)))
     )))

(define (my-append . seqs)

  (let loop ((seqs (reverse seqs))
             (result '()))
    (cond
     ((null? seqs)
      result)
     ((null? (cdr seqs))
      (append2 (car seqs)
               result))
     (else
      (loop (cdr seqs)
            (append2 (car seqs)
                     result))))))

(when
    (test/text-ui
     (make-test-suite
      "Tests for the stream stuff."

      (make-test-case
       "Tim"
       (assert-equal? (append2 '() '())  '())
       (assert-equal? (append2 '() '(a)) '(a))
       (assert-equal? (append2 '(a) '()) '(a))
       (assert-equal? (append2 '(a) '(b)) '(a b))
       (assert-equal? (append2 '(a b c) '(1 2 3)) '(a b c 1 2 3))
       )))
  
  (exit 0))
