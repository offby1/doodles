#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module alist-trie mzscheme
(provide (all-defined))
(require
 "trie.scm"
 (only "byte-vector-counter.ss" char-counts->string))

(define (how-full . args)
  (values 0 98))
(define (make-trie . args) (new-trie))
(define (is-present? at datum)
  (trie-lookup at (char-counts->string datum) (lambda () #f)))
(define (note! at datum)
  (trie-add! at (char-counts->string datum) #t)
  at)
)