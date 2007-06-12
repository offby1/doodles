#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module vtrie mzscheme
(require
 (lib "trace.ss")
 (lib "assert.ss" "offby1")
 "byte-vector-counter.ss")
(provide (rename public-note! note!)
         (rename public-make-vtrie make-vtrie)
         (rename public-is-present? is-present?)
         (rename public-how-full how-full))

(define (vtrie-print vt port write?)
  (when write? (write-string "<" port))
  (write (get-vec vt) port)
  (display " " port)
  (write (get-coi vt) port)
  (when write? (write-string ">" port)))

(define-values (s:vtrie make-vtrie vtrie? vtrie-ref vtrie-set!)
  (make-struct-type 'vtrie #f 2 0 #f
                    (list (cons prop:custom-write vtrie-print)) #f))

(define (get-vec  vtrie)   (vtrie-ref  vtrie 0))
(define (get-coi  vtrie)   (vtrie-ref  vtrie 1))
(define (set-vec! vtrie v) (check-type 'set-vec! vector? v)
                           (vtrie-set! vtrie 0 v))
(define (set-coi! vtrie c) (check-type 'set-coi! list? c)
                           (vtrie-set! vtrie 1 c))

(define (public-make-vtrie num-slots chars-of-interest)
  (make-vtrie
   (make-vector num-slots #f)
   chars-of-interest))
;(trace public-make-vtrie)

(define (public-is-present? vt count)
  (is-present? (get-vec vt) count (get-coi vt)))

(define (is-present? vec count chars-of-interest)
  (if (null? chars-of-interest)
      #t
    (let ((index (get-count (car chars-of-interest) count)))
      (if (null? (cdr chars-of-interest))
          (vector-ref vec index)
        (let ((found (vector-ref vec index)))
          (if (vector? found)
              (is-present? found count (cdr chars-of-interest))
            found)
          )))))

;(trace public-is-present?)

;; for debugging
(define (public-how-full vt)
  (how-full (get-vec vt)))
(define (how-full vec)
  (let loop ((index 0)
             (total-slots 0)
             (true-slots 0))
    (if (< index (vector-length vec))
        (let ((this (vector-ref vec index)))
          (cond
           ((vector? this)
            (let-values (((tru tot)
                          (how-full this)))
              (loop (add1 index)
                    (+ 1 total-slots tot)
                    (+ true-slots tru))
              ))
           (this (loop (add1 index)
                       (add1 total-slots)
                       (add1 true-slots)))
           (else (loop (add1 index)
                       (add1 total-slots)
                       true-slots)))
          )

      (values true-slots total-slots))))
;(trace how-full)

(define (public-note! vt count)
  (note! (get-vec vt) count (get-coi vt)))

(define (note! vec count chars-of-interest)

  (if (not (null? chars-of-interest))
      (let ((index (get-count (car chars-of-interest) count)))
        (if (null? (cdr chars-of-interest))
            (vector-set! vec index #t)
          (let ((found (vector-ref vec index)))
            (when (not (vector? found))
              (set! found  (make-vector (vector-length vec) #f)))

            (note! found count (cdr chars-of-interest))
            (vector-set!  vec index  found)))))


  ;;(printf "~a slots filled~%" (how-full vec))
  vec)
;(trace note!)
;(trace public-note!)
)
