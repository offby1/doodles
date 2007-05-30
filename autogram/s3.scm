#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#


(module s3 mzscheme

(require
 (only (lib "1.ss" "srfi") fold)
 "counter.ss")

(define a-sentence (list "This sentence contains " (cons #\a 0)))

(define (n->string n)
  "one")

(define (maybe-pluralize c n)
  (let ((s (make-string 1 c)))
    (if (= n 1)
        s
      (string-append s "s"))))

(define (template->strings t)
  (fold (lambda (thing seq)
          (if (string? thing)
              (cons thing seq)
            (let ((n (cdr thing)))
              (append seq
                      (list
                       (string-append
                        (n->string n)
                        " "
                        (maybe-pluralize (car thing)
                                         n)))))))


        '()
        t))

;; consider memoizing this.
(define (survey s)
  (let ((counts (make-count)))
    (let loop ((chars-examined 0))
      (if (= chars-examined (string-length s))
          counts
        (let ((c (string-ref s chars-examined)))
          (when (char-alphabetic? c)
            (set! c (char-downcase c))
            (inc-count! c counts))

          (loop (add1 chars-examined)))))))

(let ((c1 (make-count))
      (c2 (make-count)))
  (for-each (lambda (c) (write (char-counts->string c)) (newline) )(list c1 c2))
  (inc-count! #\a c1)
  (for-each (lambda (c) (write (char-counts->string c)) (newline) )(list c1 c2))
  (inc-count! #\a c2)
  (for-each (lambda (c) (write (char-counts->string c)) (newline) )(list c1 c2))
  (inc-count! #\x c2)
  (for-each (lambda (c) (write (char-counts->string c)) (newline) )(list c1 c2))
  (write (char-counts->string (combine-counts c1 c2))))

;; (write
;;  (fold combine-counts
;;   (make-count)
;;   (map survey
;;        (template->strings a-sentence))))

(newline)

)