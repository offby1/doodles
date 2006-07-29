;;; -*- Mode: Scheme -*-

;;;; Scsh Directory Traversal Utilities

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;; ,open handle
(define (directory-fold f dir . initial-seeds)
  (let ((dir (directory-as-file-name dir)))
    (apply values
           (fold (lambda (file seeds)
                   (call-with-values
                     (lambda ()
                       ;; Unlike DIRECTORY-FILES, DIRECTORY-FOLD will
                       ;; produce the entire file name (based on DIR,
                       ;; of course, not necessarily absolute).
                       (apply f (string-append dir "/" file) seeds))
                     list))
                 initial-seeds
                 (directory-files dir #t)))))

(define (safe-directory-fold f dir . initial-seeds)
  (let ((result (ignore-errors
                 (lambda () (apply directory-fold f dir initial-seeds))
                 )))
    (if (and (list? result)
             (not (null? result))
             (eq? 'syscall-error (car result)))
        (apply values initial-seeds)
      result))
  )

(define (directory-tree-fold f dir . initial-seeds)
  (let loop ((dir dir) (seeds initial-seeds))
    (apply safe-directory-fold
           (lambda (file . seeds)
             (receive new-seeds (apply f file seeds)
               (if (file-directory? file #f)
                   (loop file new-seeds)
                   (apply values new-seeds))))
           dir
           seeds)))

;;; Example

; (define (directory-tree-size dir)
;   (directory-tree-fold (lambda (file size)
;                          (if (file-regular? file)
;                              (+ size (file-size file))
;                              size))
;                        dir 0))

;; (directory-tree-fold (lambda (new seq)
;;                        (if (not (member ".svn" (split-file-name new)))
;;                            (cons new seq)
;;                          seq)
;;                        ) ".." '())
