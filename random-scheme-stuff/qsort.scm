;; To quicksort a list:

;; if the list has less than two elements, return the list.
;; otherwise

;; choose a pivot value.

;; for each item in the list
;;  if the item is less than the pivot, add it to "less".
;;   otherwise add it to "not less".
;; append quicksorted "less" to pivot to quicksorted "not less".


(define (qsort seq less?)
  (if (< (length seq) 2)
      seq
    (let ((pivot (car seq)))
      (let loop ((seq (cdr seq))
                 (less '())
                 (not-less '()))
        (if (null? seq)
            (append (qsort less less?) (list pivot) (qsort not-less less?))
          (if (less? (car seq) pivot)
              (loop (cdr seq)
                    (cons (car seq) less)
                    not-less)
            (loop (cdr seq)
                  less
                  (cons (car seq) not-less))))))))
