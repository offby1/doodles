#! /usr/bin/env mzscheme

#lang scheme

;$Id$

(define (rot13-char c)
  (cond
   ((char-alphabetic? c)
    (let ((upper? (char-upper-case? c))
           (c (char-downcase c)))
      ((if upper?
           char-upcase
           values)
       (integer->char (+ (char->integer #\a)
                         (remainder (+ 13 (- (char->integer c)
                                             (char->integer #\a))) 26))))))
   (else
    c)))

(define (rot13-stream in out)
  (let loop ()
    (let ((ch (read-char in)))
      (when (not (eof-object? ch))
        (display (rot13-char ch) out)
        (loop)))))

(define (by-chunks in out [chunksize 4096])
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

(by-chunks (current-input-port)
           (current-output-port))
