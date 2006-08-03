(module find mzscheme
(require (only (lib "etc.ss"     ) compose)
         (only (lib "file.ss"    ) fold-files)
         (only (lib "1.ss" "srfi") unfold))

(define (split-to-list path)
  (call-with-values
      (lambda ()(split-path path))
    list))

(define tail      (compose cadr split-to-list))
(define sans-tail (compose car  split-to-list))

(define (path->components p)
  (let ((p (simplify-path (build-path p))))
    (reverse
     (unfold (compose not path?)
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