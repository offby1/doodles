(module normals mzscheme
(provide one-unit-normal)
;; http://en.wikipedia.org/wiki/Box-Muller_transform
(define (pair-of-unit-normals)
  (let loop ((x (random))
             (y (random)))
    (let ((sum-of-squares (+ (* x x)
                             (* y y))))
      (if (or (zero? sum-of-squares)
              (< 1 sum-of-squares))
          (loop (random)
                (random))
        (let ((hmph (sqrt (/ (* -2 (log sum-of-squares))
                             sum-of-squares))))
          (list (* x hmph)
                (* y hmph)))))))

;; TODO -- find a nifty way to write this, without using assignment,
;; and without just throwing away one of the two return values from
;; pair-of-unit-normals

;; see http://schemewiki.org/view/Cookbook/NumberRecipeBiasedRands for
;; inspiration.  Executive summary: return a thunk, not a number; the
;; thunk itself returns your random number, and keeps its own little
;; cache.
(define one-unit-normal
  (let ((buffer '()))
    (lambda ()
      (when (null? buffer)
        (set! buffer (pair-of-unit-normals)))
      (begin0
        (car buffer)
        (set! buffer (cdr buffer))))))


)