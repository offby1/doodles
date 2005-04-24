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
  (if (stream-null? stream)
      stream
    (cons (stream-car stream)
          (stream->list (stream-cdr stream)))))

(define (stream-map proc seq)
  (if (stream-null? seq)
      the-null-stream
    (cons-stream (proc (car seq))
                 (stream-map proc (stream-cdr seq)))))

(define (slit . data)
  (let loop ((data (reverse data))
             (result the-null-stream))
    (if (null? data)
        result
      (loop (cdr data)
            (cons-stream (car data) result)))))

(define stream-unfold
  (let ()
    (define (something p f g seed tail-gen)
      (if (p seed)
          (tail-gen seed)
        (cons-stream (f seed)
                     (stream-unfold p f g (g seed)))))
    (case-lambda
      [(p f g seed)
       (something p f g seed (lambda (x) the-null-stream))]
      [(p f g seed tail-gen)
       (something p f g seed tail-gen)])))

#|
(define l (stream-unfold 
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
(define (stream-reduce combiner-proc init seq)
  (let loop ((seq seq)
             (result init))
    (if (stream-null? seq)
        result
      (loop (force (cdr seq))
            (combiner-proc (car seq) result)))))

(define (distribute item l)
  (cond
   ((null? l)
    '())
   ((null? (cdr l))
    (slit (list item (car l))
          (list (car l) item)))
   (else
    (cons-stream (cons item l)
                 (stream-map (lambda (seq) (cons (car l) seq))
                             (distribute item (cdr l)))))
   ))

;; ()         => ()
;; (fred)     => ((fred)     . #<promise -> ()>)
;; (fred sam) => ((fred sam) . #<promise -> ((sam fred) . #<promise -> ()>)>)
(define (stream-permute l)
  (cond
   ((stream-null? l)
    the-null-stream)
   ((stream-null? (cdr l))
    (slit l))
   (else
    (stream-apply-append
     (stream-map (lambda (seq)
                   (distribute (car l) seq))
                 (stream-permute (cdr l)))))))

;; used only by stream-append, but defined at the top level for ease
;; of testing.
(define (append2 s1 s2)
  (let loop ((s1 s1)
             (s2 s2)
             (result '()))
    (cond
     ((and (stream-null? s1)
           (stream-null? s2))
      (stream-reverse result))
     ((stream-null? s1)
      (loop (stream-cdr s2)
            '()
            (cons-stream (stream-car s2) result)))
     (else
      (loop (stream-cdr s1)
            s2
            (cons-stream (stream-car s1) result)))
     )))

(define (stream-append . streams)

  (let loop ((streams streams)
             (result '()))
    (cond
     ((null? streams)
      result)
     ((null? (cdr streams))
      (append2 result
               (car streams)))
     (else
      (loop (cdr streams)
            (append2 result
                     (car streams)))))))

(define (stream-apply-append stream-of-streams)
  
  (let loop ((streams stream-of-streams)
             (result the-null-stream))
    (cond
     ((stream-null? streams)
      result)
     ((stream-null? (stream-cdr streams))
      (append2 result
               (stream-car streams)))
     (else
      (loop (stream-cdr streams)
            (append2 result
                     (stream-car streams)))))))

(define (stream-reverse l)
  (let loop ((l l)
             (result '()))
    (if (stream-null? l)
        result
      (loop (stream-cdr l)
            (cons-stream (stream-car l) result)))))


(when
    (test/text-ui
     (make-test-suite
      "Tests for the stream stuff."

      (make-test-case
       "append2"
       (assert-equal? (stream->list (append2 (slit )         (slit )))      '())
       (assert-equal? (stream->list (append2 (slit )         (slit 'a)))    '(a))
       (assert-equal? (stream->list (append2 (slit 'a)       (slit )))      '(a))
       (assert-equal? (stream->list (append2 (slit 'a)       (slit 'b)))    '(a b))
       (assert-equal? (stream->list (append2 (slit 'a 'b 'c) (slit 1 2 3))) '(a b c 1 2 3))
       )

      (make-test-case
       "append"
       (assert-equal? (stream->list (stream-append (slit )         (slit )))      '())
       (assert-equal? (stream->list (stream-append (slit )         (slit 'a)))    '(a))
       (assert-equal? (stream->list (stream-append (slit 'a)       (slit )))      '(a))
       (assert-equal? (stream->list (stream-append (slit 'a)       (slit 'b)))    '(a b))
       (assert-equal? (stream->list (stream-append (slit 'a 'b 'c) (slit 1 2 3))) '(a b c 1 2 3))
       (assert-equal? (stream->list (stream-append (slit 'a)
                                                   (slit 'b)
                                                   (slit 'c)
                                                   (slit 1)
                                                   (slit 2)
                                                   (slit 3))) '(a b c 1 2 3))
       )

      (make-test-case
       "apply-append"
       (assert-equal? (stream->list (stream-apply-append (slit (slit )         (slit ))))      '())
       (assert-equal? (stream->list (stream-apply-append (slit (slit )         (slit 'a))))    '(a))
       (assert-equal? (stream->list (stream-apply-append (slit (slit 'a)       (slit ))))      '(a))
       (assert-equal? (stream->list (stream-apply-append (slit (slit 'a)       (slit 'b))))    '(a b))
       (assert-equal? (stream->list (stream-apply-append (slit (slit 'a 'b 'c) (slit 1 2 3)))) '(a b c 1 2 3))
       (assert-equal? (stream->list (stream-apply-append (slit (slit 'a)
                                                               (slit 'b)
                                                               (slit 'c)
                                                               (slit 1)
                                                               (slit 2)
                                                               (slit 3)))) '(a b c 1 2 3))
       )

      (make-test-case
       "reverse"
       (assert-equal? (stream->list (stream-reverse (slit ))) '())
       (assert-equal? (stream->list (stream-reverse (slit 1))) '(1))
       (assert-equal? (stream->list (stream-reverse (slit 1 2 3))) '(3 2 1))
       )

      (make-test-case
       "map"
       (assert-equal? (stream->list (stream-map add1 (slit))) '())
       (assert-equal? (stream->list (stream-map add1 (slit 9))) '(10))
       (assert-equal? (stream->list (stream-map add1 (slit 9 10))) '(10 11))
       )

      (make-test-case
       "distribute"
       (assert-equal? (stream->list (distribute 3 (list         ))) '())
       (assert-equal? (stream->list (distribute 3 (list 'a      ))) '((3 a    ) (a 3    )                    ))
       (assert-equal? (stream->list (distribute 3 (list 'a 'b   ))) '((3 a b  ) (a 3 b  ) (a b 3  )          ))
       (assert-equal? (stream->list (distribute 3 (list 'a 'b 'c))) '((3 a b c) (a 3 b c) (a b 3 c) (a b c 3)))
       )

      (make-test-case
       "permute"
       (assert-equal? (stream->list (stream-permute (list))) '())
       (assert-equal? (stream->list (stream-permute (list 1))) '((1)))
       
       (assert-true
        (or (equal? (stream->list (stream-permute (list 1 2)))  '((1 2) (2 1)))
            (equal? (stream->list (stream-permute (list 1 2)))  '((2 1) (1 2))))))
      ))
  
  (exit 0))
