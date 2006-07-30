;;; -*- Mode: Scheme -*-

;;;; Scsh Directory Traversal Utilities

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;; ,open handle conditions
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

(define (ok fn)
  (not (member ".svn" (split-file-name fn))))

(directory-tree-fold ok
                     (lambda (new seq)
                       (if (ok new)
                           (cons new seq)
                         seq))

                     "/tmp/wc" '())

(directory-tree-fold values cons "/tmp/" '())
