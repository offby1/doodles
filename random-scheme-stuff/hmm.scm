#!/usr/local/bin/guile -s
!#

;; displays 100 lines of text that look like they came from PGP.

;; Godawfully slow ... I wonder why?

(define (random-lines)
  (define (random-line)
    (define (random-character)
      (define (integer->character i)
        (define characters
          "+/01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (string-ref characters (remainder i (string-length characters))))
      (integer->character (char->integer (with-input-from-file
                                             "/dev/urandom" peek-char))))
    (define result (make-string 64))
    (let loop ((chars 0))
      (if (< chars (string-length result))
          (begin
            (string-set! result chars (random-character))
            (loop (+ 1 chars)))
        result)))
  (let loop ((lines 0)
             (result '()))
    (if (< lines 100)
        (loop (+ 1 lines)
              (cons (random-line) result))
      result)))

(random-lines)