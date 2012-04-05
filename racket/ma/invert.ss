#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module invert mzscheme
  (provide invert)
  (require  (planet "test.ss" ("schematics" "schemeunit.plt" 2))
            (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
            (lib "match.ss"))

  ;; stolen from Aubrey Jaffer's slib

  ;;@args modulus
  ;;Returns the non-negative integer characteristic of the ring formed when
  ;;@var{modulus} is used with @code{modular:} procedures.
  (define (modulus->integer m)
    (cond ((negative? m) (- 1 m m))
          ((zero? m) #f)
          (else m)))

  ;;@args n1 n2
  ;;Returns a list of 3 integers @code{(d x y)} such that d = gcd(@var{n1},
  ;;@var{n2}) = @var{n1} * x + @var{n2} * y.
  (define (extended-euclid x y)
    (define q 0)
    (do ((r0 x r1) (r1 y (remainder r0 r1))
         (u0 1 u1) (u1 0 (- u0 (* q u1)))
         (v0 0 v1) (v1 1 (- v0 (* q v1))))
        ((zero? r1) (list r0 u0 v0))
      (set! q (quotient r0 r1))))

  ;;@args modulus n
  ;;Returns the integer @code{(modulo @var{n} (modulus->integer
  ;;@var{modulus}))} in the representation specified by @var{modulus}.
  (define normalize
    (lambda (m k)
      (cond ((positive? m) (modulo k m))
            ((zero? m) k)
            ((<= m k (- m)) k)
            (else
             (let* ((pm (+ 1 (* -2 m)))
                    (s (modulo k pm)))
               (if (<= s (- m))
                   s
                 (- s pm)))))))

  ;;@args modulus n2
  ;;Returns an integer n such that 1 = (n * @var{n2}) mod @var{modulus}.  If
  ;;@var{n2} has no inverse mod @var{modulus} an error is signaled.
  (define (invert a m)
    (cond ((eqv? 1 (abs a)) a)		; unit
          (else
           (let ((pm (modulus->integer m)))
             (cond
              (pm
               (let ((d (extended-euclid (normalize pm a) pm)))
                 (if (= 1 (car d))
                     (normalize m (cadr d))
		   (error 'invert "can't invert" m a))))
              (else (error 'invert "can't invert" m a)))))))

  (define (one-euclid-test-case  a b)
    (test-case
     "just one case, ma'am"
     (match-let (((r p s) (extended-euclid a b)))
       (check-equal? r (+ (* p a) (* s b)))
       )))
  (when
      (not
       (test/text-ui
        (test-suite
         "Tests for modular invert."
         (test-case
          "modulus->integer"
          (check = 3 (modulus->integer 3))
          (check = 7 (modulus->integer -3))
          (check-false (modulus->integer 0)))

         (one-euclid-test-case 81 57)
         (one-euclid-test-case 20 -19)

         (test-case
          "normalize"
          (check = 3 (normalize 7 10))
          (check = 3 (normalize 7 24))
          (check = 3 (normalize 7 -4))
          )

         (test-case
          "invert"
          (check = 1 (invert 11 5))
          (check = 1 (invert 1 5))
          (check = 3 (invert 2 5))
          (check = 2 (invert 3 5))
          (check = 4 (invert 4 5))
          )
         )))
    (exit 1))
  )