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
  (hash-table-put! (char-counts-ht counter) char (get-count char counter)))

;; consider memoizing this.
(define (survey s)
  (let ((counts '()))
    (let loop ((chars-examined 0))
      (if (= chars-examined (string-length s))
          counts
        (let ((c (string-ref s chars-examined)))
          (when (char-alphabetic? c)
            (set! c (char-downcase c))
            (let ((probe (assq c counts)))
              (when (not probe)
                (set! probe (cons c 0))
                (set! counts (cons probe counts))
                (for-each display (list "Counts is " counts #\newline)))
              (set-cdr! probe (add1 (cdr probe)))))

          (loop (add1 chars-examined)))))))

(define (combine-counts alists)
  alists)

(write
 (let ((all-counts (map (lambda (s)
                          (survey s))
                        (template->strings a-sentence))))
   (combine-counts all-counts)))

)