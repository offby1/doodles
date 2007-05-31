#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#


(module s3 mzscheme

(require
 (only (lib "1.ss" "srfi")
       filter
       fold)
 (planet "memoize.ss" ("dherman" "memoize.plt" 2 1))
 (planet "numspell.ss" ("neil" "numspell.plt" 1 0))
 "counter.ss")

(define a-template (list
                    "This sentence contains "
                    (cons #\a 0)
                    " as well as "
                    (cons #\b 0)

;;                     ", " (cons #\c 0)
;;                     ", " (cons #\d 0)
                    ", " (cons #\e 0)
;;                     ", " (cons #\f 0)
;;                     ", " (cons #\g 0)
;;                     ", " (cons #\h 0)
;;                     ", " (cons #\i 0)
;;                     ", " (cons #\j 0)
;;                     ", " (cons #\k 0)
;;                     ", " (cons #\l 0)
;;                     ", " (cons #\m 0)
;;                     ", " (cons #\n 0)
                     ", " (cons #\o 0)
;;                     ", " (cons #\p 0)
;;                     ", " (cons #\q 0)
;;                     ", " (cons #\r 0)
;;                     ", " (cons #\s 0)
                     ", " (cons #\t 0)
;;                     ", " (cons #\u 0)
;;                     ", " (cons #\v 0)
;;                     ", " (cons #\w 0)
;;                     ", " (cons #\x 0)
;;                     ", " (cons #\y 0)
                    ", and " (cons #\z 0)
                    "."
                    ))

(define (maybe-pluralize s n)
  (if (= n 1)
      s
    (string-append s "s")))

(define (update-template t counts)
  (reverse
   (fold (lambda (thing so-far)
           (if (string? thing)
               (cons thing so-far)
             (cons (cons  (car thing) (get-count (car thing) counts))
                   so-far)))
         '()
         t)))
(define (randomly-seed t)
  (reverse
   (fold (lambda (thing so-far)
           (if (string? thing)
               (cons thing so-far)
             (cons (cons (car thing)
                         (random 100))
                   so-far)))
         '()
         t)))

;; memoization seems pointless, since if we're searching for truths,
;; we should never call this twice on the same template.
(define (template->counts t)
  (define (survey s)
    (let ((counts (make-count)))
      (let loop ((chars-examined 0))
        (if (= chars-examined (string-length s))
            counts
          (let ((c (string-ref s chars-examined)))
            (when (char-alphabetic? c)
              (inc-count! (char-downcase c) counts))

            (loop (add1 chars-examined)))))))
  (fold
   combine-counts
   (make-count)
   (map survey
        (template->strings t))))

(define (template->strings t)
  (reverse
   (fold (lambda (thing so-far)
           (if (string? thing)
               (cons thing so-far)
             (let ((n (cdr thing)))
               (cons (string-append
                      (number->english n)
                      " "
                      (maybe-pluralize
                       (string-append "'"
                                      (make-string 1 (car thing))
                                      "'")
                       n))
                     so-far))))


         '()
         t)))

(define (just-the-conses seq)
  (filter pair? seq))

(define (make-calm-notifier thunk)
  (define (is-power-of-two? x)
    (or (= 1 x)
        (and (not (odd? x))
             (is-power-of-two? ( / x 2)))))
  (let ((invocation-count 0))
    (lambda ()
      (set! invocation-count (add1 invocation-count))
      (if (is-power-of-two? invocation-count)
          (thunk)))))

(let* ((seen (make-hash-table 'equal))
       (hash-table-grew-feedback
        (make-calm-notifier
         (lambda ()
           (fprintf
            (current-error-port)
            "We've examined ~a variations.~%"
            (number->english (hash-table-count seen)))))))
  (let loop ((t a-template))
    (let ((next (update-template t (template->counts t))))
      ;; this assumes that templates will always have keys in the
      ;; same order.
      (if (equal? next t)
          (printf "We got a winner: ~s~%" (apply string-append (template->strings t)))
        (let ((key (just-the-conses t)))
          (if (hash-table-get seen key #f)
              (loop (randomly-seed a-template))
            (begin
              (hash-table-put! seen key #t)
              (hash-table-grew-feedback)
              (loop next)))))))))
