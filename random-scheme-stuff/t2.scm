#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module t2 mzscheme
(require (only (lib "1.ss" "srfi") fold iota)
         (only (lib "13.ss" "srfi") string-join)
         (only (lib "list.ss") quicksort)
         "normals.ss")

(print-struct #t)
(define-struct item (name size) #f)
(define-struct container (items remaining-capacity) #f)

(define *containers* (map (lambda (ignored)
                            (make-container '() 0))
                          (iota 5)))

(define (emptiest containers)
  (fold
   (lambda (c1 c2)
     (if (>  (container-remaining-capacity c1)
             (container-remaining-capacity c2))
         c1
       c2))
   (car containers) containers))

(define (container-add! c i)
  (set-container-items! c (cons i (container-items c)))
  (set-container-remaining-capacity! c (-
                                        (container-remaining-capacity
                                         c)
                                        (item-size i))))
(define *random-items*
  (map (lambda (n)
         (make-item (number->string n) (inexact->exact (round (* (one-unit-normal) 100)))))
       (iota 50)))

(for-each (lambda (i)
            (let ((c (emptiest *containers*)))
              (container-add! c i)))
          (quicksort
           *random-items*
           (lambda (i1 i2)
             (> (item-size i1)
                (item-size i2)))))
;(printf "~a~%" (map item-size *random-items*))
(for-each (lambda (c)
            (let ((sizes (map item-size (container-items c))))
            (printf "~a = ~a~%"
                    (apply + sizes)
                    (string-join
                     (map number->string (reverse sizes))
                     " + "
                     'infix))
            (newline)))
           *containers*)
(newline)
)
