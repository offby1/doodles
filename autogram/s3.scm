#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#


(module s3 mzscheme

(require
 (only (lib "1.ss" "srfi") fold))

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

(define-struct char-counts (ht) #f)
(define (get-count char counter)
  (hash-table-get (char-counts-ht counter) char 0))
(define (inc-count! char counter)
  (hash-table-put! (char-counts-ht counter) char (add1 (get-count char counter))))
(define (char-counts->string cc)
  (format "~a" (hash-table-map (char-counts-ht cc) cons)))

;; consider memoizing this.
(define (survey s)
  (let ((counts (make-char-counts (make-hash-table))))
    (let loop ((chars-examined 0))
      (if (= chars-examined (string-length s))
          counts
        (let ((c (string-ref s chars-examined)))
          (when (char-alphabetic? c)
            (set! c (char-downcase c))
            (inc-count! c counts))

          (loop (add1 chars-examined)))))))

(define (combine-counts alists)
  alists)

(for-each (lambda (s)
            (write (char-counts->string (survey s)))
            )
     (template->strings a-sentence))

(newline)

)