#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

;; given a bunch of items of random sizes, and a bunch of identical
;; empty containers, put the items into the containers in such a way
;; as to fill each container as full as each other (as close as
;; possible).

;; Do this by sorting the items in descending order by size, then for
;; each item, add it to the emptiest container.

;; see "triumph.scm" for a longer explanation of why this is
;; interesting (to me, anyway).  That file says that it took me
;; _hours_ to write it, which is appalling; this here took maybe half
;; an hour.  Perhaps I'm a better programmer now :-|.
(module t2 mzscheme
(require (only (lib "1.ss" "srfi") fold iota)
         (only (lib "13.ss" "srfi") string-join)
         (only (lib "list.ss") sort)
         "normals.ss")

(define-struct item (name size) #f)
(define-struct container (items remaining-capacity) #f)

(define *containers* (map (lambda ignored
                            (make-container '() 0))
                          (iota 8)))

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
  (set-container-remaining-capacity!
   c
   (-
    (container-remaining-capacity c)
    (item-size i))))

(define *random-items*
  (map (lambda (n)
         (make-item (number->string n) (inexact->exact (round (* (one-unit-normal) 100)))))
       (iota 3000)))

;; pack the items into the containers.
(for-each (lambda (i)
            (let ((c (emptiest *containers*)))
              (container-add! c i)))
          (sort
           *random-items*
           (lambda (i1 i2)
             (> (item-size i1)
                (item-size i2)))))

;(printf "~a~%" (map item-size *random-items*))

;; now show the packed containers.
(for-each (lambda (c)
            (let ((sizes (map item-size (container-items c))))
              (printf "~a = ~a~%"
                      (apply + sizes)
                      (string-join
                       (map number->string (reverse sizes))
                       " + "
                       'infix))))
          (sort
           *containers*
           (lambda (c1 c2)
             (> (container-remaining-capacity c1)
                (container-remaining-capacity c2)))))
(newline)
)
