#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#


(module s3 mzscheme

(require
 (only (lib "1.ss" "srfi") fold)
 (planet "numspell.ss" ("neil" "numspell.plt" 1 0))
 "counter.ss")

(define a-sentence (list
                    "This sentence contains "
                    (cons #\a 0)
                    " as well as "
                    (cons #\x 0)))

(define (maybe-pluralize c n)
  (let ((s (make-string 1 c)))
    (if (= n 1)
        s
      (string-append s "s"))))

;; general idea: template -> counts -> updated template
(define (update-template t counts)
  (reverse
   (fold (lambda (thing so-far)
           (if (string? thing)
               (cons thing so-far)
             (cons (cons  (car thing) (get-count (car thing) counts))
                   so-far)))
         '()
         t)))


(define (template->counts t)
  (fold
   combine-counts
   (make-count)
   (map survey
        (template->strings t)))
  )

(define (template->strings t)
  (reverse
   (fold (lambda (thing so-far)
           (if (string? thing)
               (cons thing so-far)
             (let ((n (cdr thing)))
               (cons (string-append
                      (number->english n)
                      " "
                      (maybe-pluralize (car thing)
                                       n))
                     so-far))))


         '()
         t)))

;; consider memoizing this.
(define (survey s)
  (let ((counts (make-count)))
    (let loop ((chars-examined 0))
      (if (= chars-examined (string-length s))
          counts
        (let ((c (string-ref s chars-examined)))
          (when (char-alphabetic? c)
            (inc-count! (char-downcase c) counts))

          (loop (add1 chars-examined)))))))

(let ((counts (template->counts a-sentence)))
  (printf
   "~a:~%~a -> ~a~%"
   (apply string-append (template->strings a-sentence))
   (char-counts->string counts)
   (apply string-append (template->strings (update-template a-sentence counts)))))

)