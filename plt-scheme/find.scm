(module find mzscheme
(require (lib "file.ss")
         (lib "1.ss" "srfi"))

(define (sans-tail path)
  (let-values (((base name must-be-dir?)
                (split-path path)))
    base))

(define (path->components p)
  (let ((p (simplify-path
            (if (not (path? p))
                (build-path p)
              p))))
    (reverse
     (unfold (lambda (thing)
               (not (path? thing)))
             tail
             sans-tail
             p))))

(define (is-svn? path)
  (member (build-path ".svn")
          (path->components path)))

(fold-files
 (lambda (path flavor seeds)
   (if (is-svn? path)
       seeds
     (cons path seeds)))
 '())
)