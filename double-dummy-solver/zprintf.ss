#lang scheme

(provide (all-defined-out))

(define *recursion-level* (make-parameter 0))
(define *really-loud* (make-parameter #f))
(define *shaddap* (make-parameter #f))

(port-count-lines! (current-output-port))
(define (zprintf . args)
  (when (and (not (*shaddap*))
             (or (*really-loud*)
                 (zero? (*recursion-level*))))
    (let-values (((line col pos)
                  (port-next-location (current-output-port))))
      (when (and col (positive? col))
        (display " ")))
    (apply printf args)
    (flush-output)))

;; handy for debugging -- wrap it around a call like this

;; (zp "is ~a held by ~a or ~a? ~a~%" c lho rho
;;     (or (member c (ha:cards lho))
;;         (member c (ha:cards rho))))

(define (zp . args)
  (apply zprintf args)
  (car (last-pair args)))
(define (p . args)
  (apply printf args)
  (flush-output)
  (car (last-pair args)))
