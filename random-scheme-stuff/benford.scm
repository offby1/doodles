#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

;; attempt to demonstrate Benford's Law
;; (http://www.rexswain.com/benford.html;
;; http://en.wikipedia.org/wiki/Benford%27s_law)

(module benford mzscheme
(require "normals.ss")
(require "linear-regression.scm")

(define (increment! datum)
  (hash-table-put! *stats* datum (+ 1 (hash-table-get *stats* datum (lambda () 0)))))

(define (first-digit x)
  (let loop ((x x))
    (cond
     ((zero? x)
      0)
     ((negative? x)
      (loop (- x)))
     ((< x 1)
      (loop (* x 10)))
     ((>= x 10)
      (loop (/ x 10)))
     (else
      (inexact->exact (truncate x))))))

(define (log10 x)
  (/ (log x) (log 10)))

(define (ten-to-the x)
  (expt 10 x))

(define *passes* 100000)
(define *stats* (make-hash-table 'equal))
(let loop ((experiments-left *passes*))
  (if (positive? experiments-left )
      (let ((datum (first-digit (one-unit-normal))))
        (increment! datum)
        (loop (- experiments-left 1)))
    (let ((barchart-data (reverse (hash-table-map *stats* cons))))

      ;; the expected value for each X is  (* *passes* (log10 (+ 1 (/ x))))
      (define (invert actual)
        (/ (- (ten-to-the (/ actual *passes*)) 1)))

      (write barchart-data)
      (newline)
      (printf
       "correlation coefficient: ~a~%"
       (list-ref
        (find-best-fit-line
         (map
          (lambda (p)
            (let* ((digit (car p))
                   (occurrences (cdr p)))
              (cons digit (invert occurrences))))

          barchart-data))
        2))))))
