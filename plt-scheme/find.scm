(module find mzscheme
(require (only (lib "file.ss"    ) fold-files)
         (only (lib "1.ss" "srfi") unfold))

(define (tail path)
  (let-values (((base name must-be-dir?)
                (split-path path)))
    name))

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

(define (dn thing)
  (display thing)
  (newline))

(for-each dn
          (fold-files
           (lambda (path flavor seeds)
             (fprintf (current-error-port) "Considering ~s~%" path)
             (if (is-svn? path)
                 seeds
               (cons path seeds)))
           '()))
)