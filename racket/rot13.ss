#! /usr/bin/env mzscheme

#lang scheme

;$Id$

(define (rot13-char c)
  (cond

   ;; if it's a digit, do rot10, just for fun
   ((char-numeric? c)
    (let ((n (- (char->integer c)
                (char->integer #\0))))
      (integer->char
       (+ (char->integer #\0)
          (remainder (+ 5 n) 10)))))

   ((char-alphabetic? c)
    (let ((upper? (char-upper-case? c))
           (c (char-downcase c)))
      ((if upper?
           char-upcase
           values)
       (let ((n (- (char->integer c)
                   (char->integer #\a))))
         (integer->char (+ (char->integer #\a)
                           (remainder (+ 13 n) 26)))))))
   (else
    c)))

(define (rot13-stream in out [chunksize 4096])
  (with-handlers

      ;; swallow a "broken pipe" exception, in case our output is
      ;; being piped to "head" or something.
      ([exn:fail:filesystem? (lambda (e) (values))])

    (let loop ()
      (let ((block (read-bytes chunksize in)))
        (when (not (eof-object? block))
          (for ([byte (in-bytes block)])
               (display (rot13-char (integer->char byte))
                        out))
          (loop))))))

(rot13-stream (current-input-port)
              (current-output-port))
