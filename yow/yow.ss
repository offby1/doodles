#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5748 2008-11-17 01:57:34Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(define (offset-of-first-nul-byte ip)
  (let loop ([offset 0])
    (file-position ip offset)
    (if (zero? (peek-byte ip))
        offset
        (loop (add1 offset)))))

(define (bytes-until-next-nul ip)
  (list-ref
   (regexp-match #rx"(.*?)\000" ip)
   1))

(define (main . args)
  (let ([fn "yow.lines"])
    (call-with-input-file fn
      (lambda (ip)

        ;; The size in bytes of all the quotes, including their trailing
        ;; 0 byte.  Basically we're skipping the initial comment (and
        ;; its 0 byte).
        (define first-quote-byte (add1 (offset-of-first-nul-byte ip)))
        (define size (- (file-size fn) first-quote-byte))

        (define starting-point
          (+ (random size) first-quote-byte))

        ;; Pick a random spot in the file.
        (file-position ip starting-point)

        ;; Seek backwards until we see a zero byte.
        (let loop ()
          (let ((b (peek-byte ip)))
            (when (not (zero? b))
              (file-position ip (sub1 (file-position ip)))
              (loop))))

        (file-position ip (add1 (file-position ip)))

        (display (bytes-until-next-nul ip))
        (newline)
        (flush-output)
        ))))

(provide (all-defined-out))
