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

;; consider memoizing this.  Alas, it modifies "counts".
(define (survey s counts)
  (let loop ((chars-examined 0))
    (if (= chars-examined (string-length s))
        counts
      (let ((c (string-ref s chars-examined)))
        (when (char-alphabetic? c)
          (set! c (char-downcase c))
          (let ((probe (assq c (unbox counts))))
            (when (not probe)
              (set! probe (cons c 0))
              (set-box! counts (cons probe (unbox counts)))
              (for-each display (list "Counts is " (unbox counts) #\newline)))
            (set-cdr! probe (add1 (cdr probe)))))

        (loop (add1 chars-examined))))))

(let ((counts (box '())))
  (write (map (lambda (s)
                (unbox (survey s counts)))
              (template->strings a-sentence))))

)