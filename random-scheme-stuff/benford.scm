;; attempt to demonstrate Benford's Law
;; (http://www.rexswain.com/benford.html)

(module benford mzscheme
(require "normals.ss")

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

(define log10
  (let ((log-of-10 (log 10)))
    (lambda (x)
      (/ (log x)
         log-of-10))))

(define *passes* 100000)
(define *stats* (make-hash-table 'equal))
(let loop ((experiments-left *passes*))
  (if (positive? experiments-left )
      (let ((datum (first-digit (one-unit-normal))))
        (increment! datum)
        (loop (- experiments-left 1)))
    (for-each display
              (hash-table-map
               *stats*
               (lambda (key value)
                 (format "Digit: ~a  Expected: ~a Actual: ~a~%"
                         key
                         (inexact->exact (round (* *passes* (log10 (+ 1 (/ key))))))
                         value))))))
)