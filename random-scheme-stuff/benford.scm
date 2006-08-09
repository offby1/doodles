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

(define *stats* (make-hash-table 'equal))
(define (increment! datum)
  (hash-table-put! *stats* datum (+ 1 (hash-table-get *stats* datum (lambda () 0)))))

(define (first-digit x)
  (cond
   ((zero? x)
    0)
   ((negative? x)
    (first-digit (- x)))
   ((< x 1)
    (first-digit (* x 10)))
   ((>= x 10)
    (first-digit (/ x 10)))
   (else
    (inexact->exact (truncate x)))))

(define (log10 x)
  (/ (log x) (log 10)))

(define (ten-to-the x)
  (expt 10 x))

(define *passes* 1000000)

(let loop ((experiments-left *passes*))
  (if (positive? experiments-left )
      (begin
        (increment! (first-digit (one-unit-normal)))
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
