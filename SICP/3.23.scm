#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

;; page 266

(define (make-dll)
  (list '()))

(define (triple a b c)
  (cons a (cons b c)))
(define one   car )
(define two   cadr)
(define three cddr)

(define set-one! set-car!)
(define (set-two! trip obj)
  (set-car! (cdr trip) obj))
(define (set-three! trip obj)
  (set-cdr! (cdr trip) obj))

;;;

(define (make-dll) (list '()))
(define (dll-prepend dll obj)
  (let ((front dll)
        )))

(define (make-deque)
  (list '()))

(define (empty-deque? x)
  (and (pair? x)
       (null? (car x))
       (null? (cdr x))))

(define front-deque caar)
(define rear-deque cadr)

(define (front-insert-deque! obj q)
  )