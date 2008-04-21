#! /usr/bin/env mzscheme

#lang scheme

;$Id$

(define (rot13 string)
  (let ((chars (string->list string)))
    (list->string
     (map (lambda (c)
            (cond
             ((char-alphabetic? c)
              (let* ((upper? (char-upper-case? c))
                     (c (char-downcase c)))
                ((if upper?
                     char-upcase
                     values)
                 (integer->char (+ (char->integer #\a)
                                   (remainder (+ 13 (- (char->integer c)
                                                       (char->integer #\a))) 26))))))
             (else
              c)
             ))
          (string->list string)))))



