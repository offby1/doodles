#! /bin/sh
#|
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require (lib "1.ss" "srfi"))
(require (lib "pretty.ss"))

(define (name k)
  (car k))

(define (values k)
  (cadr k))

(define (lowest-val k)
  (car (values k)))

(define (at-lowest k)
  (list (name k)
        (car (values k))))

(define (exclude-lowest k)
  (list (name k)
        (cdr (values k))))

(define (interesting-settings knobs)
  
  (append
   
   (list
    ;; First, all the knobs at their lowest setting.
    (map (lambda (k)
           (list (name k)
                 (lowest-val k)))
         knobs))
  
   ;; Now, for each knob that has more than one setting ...
   (append-map
    (lambda (k)
      (let ((other-knobs (filter
                          (lambda (o)
                            (not (eq? (name o)
                                      (name k))))
                          knobs)))

        ;; for each of our other values
        (map (lambda (v)
               
               ;; a setting with us at that value, and everyone else
               ;; at their lowest.
               (cons
                (list (name k) v)
                (map at-lowest other-knobs)))
                 
             (values  k))))
   
    (filter (lambda (k) (not (null? (values k))))
            (map exclude-lowest knobs)))))

(pretty-print (interesting-settings '((trustees  (1 2 5 6))
                                      (voters    (1 2 10 11 30 31 100 101))
                                      (questions (1 2 10 11 30 31))
                                      (answers   (1 2 10 11 30 31)))))
