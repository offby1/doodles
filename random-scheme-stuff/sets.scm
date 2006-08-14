;; a set is a list, no element of which is eq? to any other element.

;; it'd probably be best if none of the elements were pairs or
;; vectors, either.

(define (common-items l1 l2)
  (cond
   ((null? l1)
    '())
   ((null? l2)
    '())
   ((memq (car l1)
             l2)
    (cons (car l1)
          (common-items (cdr l1)
                        l2)))
   ((memq (car l2)
             l1)
    (cons (car l2)
          (common-items l1
                        (cdr l2))))
   (#t
    (common-items (cdr l1)
                  (cdr l2)))))

;; return a list of those elements of l1 that don't also appear in l2.
(require 'filter)
(define (subtract l1 l2)
  (filter (lambda (item)
            (not (memq item l2)))
          l1))
