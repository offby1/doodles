#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module vtrie mzscheme
(require
 (lib "assert.ss" "offby1")
 (lib "trace.ss")
 (planet "test.ss"     ("schematics" "schemeunit.plt" 2))
 (planet "text-ui.ss"  ("schematics" "schemeunit.plt" 2))
 (planet "util.ss"     ("schematics" "schemeunit.plt" 2))
 "byte-vector-counter.ss")

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
(trace public-make-vtrie)
(define *chars-of-interest* (string->list "boxiest"))

(define (public-is-present? vt count)
  (is-present? vt count (get-coi vt)))

(define (is-present? vt count chars-of-interest)
  (if (null? chars-of-interest)
      #t
    (let ((index (get-count (car chars-of-interest) count)))
      (if (null? (cdr chars-of-interest))
          (vector-ref vt index)
        (let ((found (vector-ref (get-vec vt) index)))
          (if (vector? found)
              (is-present? found count (cdr chars-of-interest))
            found)
          )))))

(trace public-is-present?)

(define (public-add! vt count)
  (set-vec! vt (add! vt count (get-coi vt)))
  vt)

(define (add! vt count chars-of-interest)
  (when (not (null? chars-of-interest))
    (let ((index (get-count (car chars-of-interest) count)))
      (if (null? (cdr chars-of-interest))
          (vector-set! (get-vec vt) index #t)
        (let ((found (vector-ref (get-vec vt) index)))
          (if (vector? found)
              (add! found count (cdr chars-of-interest))
            (let ((new (public-make-vtrie (vector-length (get-vec vt)) chars-of-interest)))
              (add! new count (cdr chars-of-interest))
              (vector-set! (get-vec vt) index new)
              ))))))
  (get-vec vt))
(trace public-add!)

(exit-if-failed
 (let ((c1 (make-count *chars-of-interest*))
       (c2 (make-count *chars-of-interest*))
       (min 1)
       (max 14))
   (test/text-ui
    (test-suite
     "The one and only suite"

     #:before
     (lambda ()
       (inc-count! #\b c1 2)
       (inc-count! #\x c2 3))

;;      (test-false
;;       "empty"
;;       (let ((vt (public-make-vtrie max  *chars-of-interest*)))
;;         (public-is-present? vt c1)))

     (test-case
      "adding"
      (let ((vt (public-make-vtrie max *chars-of-interest*)))
        (public-add! vt c1)
        (check-not-false
         (public-is-present? vt c1))

        (public-add! vt c2)
        (check-not-false
         (and
          (public-is-present? vt c1)
          (public-is-present? vt c2)))))
     ))))

)
