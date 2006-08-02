#!/usr/local/bin/scsh \
-o handle -o conditions -s
!#

;;; -*- Mode: Scheme -*-

;;;; Scsh Directory Traversal Utilities

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (directory-fold f dir . initial-seeds)
  (let ((dir (directory-as-file-name dir)))
    (apply values
           (fold (lambda (file seeds)
                   (call-with-values
                       (lambda ()
                         ;; Unlike DIRECTORY-FILES, DIRECTORY-FOLD will
                         ;; produce the entire file name (based on DIR,
                         ;; of course, not necessarily absolute).
                         (apply
                          f
                          (string-append
                           dir
                           (if (string=? dir "/") "" "/")
                           file)
                          seeds))
                     list))
                 initial-seeds
                 (directory-files dir #t)))))

(define (safe-directory-fold f dir . initial-seeds)
  (define (syscall-error? thing)
    (and (error? thing)
         (eq? 'syscall-error (car thing))))
  (call-with-current-continuation
   (lambda (abort)
     (with-handler
      (lambda (condition propagate)
        (if (syscall-error? condition)
            (abort (apply values initial-seeds))
          (propagate)))
      (lambda () (apply directory-fold f dir initial-seeds))))))

(define (directory-tree-fold descend? f dir . initial-seeds)
  (let loop ((dir dir) (seeds initial-seeds))
    (apply safe-directory-fold
           (lambda (file . seeds)
             (receive new-seeds (apply f file seeds)
               (if (and (file-directory? file #f)
                        (descend? file))
                   (loop file new-seeds)
                 (apply values new-seeds))))
           dir
           seeds)))

;;; Examples

                                        ; (define (directory-tree-size dir)
                                        ;   (directory-tree-fold (lambda (file size)
                                        ;                          (if (file-regular? file)
                                        ;                              (+ size (file-size file))
                                        ;                              size))
                                        ;                        dir 0))

;;; find all files except .svn and its children
;; (directory-tree-fold (lambda (new seq)
;;                        (if (not (member ".svn" (split-file-name new)))
;;                            (cons new seq)
;;                          seq)
;;                        ) ".." '())

(define (pruning-tree-fold f dir . initial-seeds)
  (define (ok fn)
    (not (member ".svn" (split-file-name fn))))

  (define (cons-if-ok new seq)
    (if (ok new)
        (cons new seq)
      seq))

  (define (and-compose f g)
    (lambda args
      (and (apply g args)
           (apply f args))))
  (apply directory-tree-fold
         ok
         (and-compose cons-if-ok f)
         dir
         initial-seeds))

;; (for-each (lambda (x)
;;             (display x)
;;             (newline)) (pruning-tree-fold cons "/tmp/wc" '()))
