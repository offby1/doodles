#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module alist-trie mzscheme
(provide (all-defined))
(define-struct alist-trie (alist) #f)
(define (how-full . args)
  (values 0 98))
(define (make-trie . args)
  (make-alist-trie '()))
(define (is-present? at datum)
  (member datum (alist-trie-alist at)))
(define (note! at datum)
  (set-alist-trie-alist! at (cons datum (alist-trie-alist at))))
)