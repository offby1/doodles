#lang racket
; Hey Emacs, this is -*-scheme-*- code!

(provide (all-defined-out))

;; A finite stream of directories, starting at bottom-directory,
;; ending with the root directory.
(struct parent-directory-stream (bottom-directory)
  #:guard (lambda (dir struct-name)
            (cond
             [(file-exists? dir)
              ;; If dir is really a file, return its directory.
              (let-values ([(base name must-be-dir?) (split-path dir)])
                (if (eq? base 'relative)
                    "."
                    base))]
             [(not (directory-exists? dir))
               (error struct-name "~a is not a directory" dir)]

             [#t
              (build-path dir)]
             ))
  #:transparent
  #:methods gen:stream
  [(define (stream-empty? stream)
     (not (directory-exists? (parent-directory-stream-bottom-directory stream))))
   (define (stream-first stream)
     (parent-directory-stream-bottom-directory stream))
   (define (stream-rest stream)
     (let* ([d (parent-directory-stream-bottom-directory stream)]
            [p (build-path d "..")])
       (if (equal? (file-or-directory-identity d)
                   (file-or-directory-identity p))
           (list)
           (parent-directory-stream p))))])

(define (find-parent-dir starting-dir criterion)
  (for/first ([d (parent-directory-stream starting-dir)]
              #:when (criterion d))
    d))

(define (find-parent-containing starting-dir sentinel)
  (find-parent-dir
   starting-dir
   (lambda (d)
     (let ([sentinel (build-path d sentinel)])
       (or (file-exists? sentinel)
           (directory-exists? sentinel))))))

;; e.g.
;; (find-parent-containing
;;  "/Users/erichanchrow/git-repositories/Me/doodles/racket/dirstream.rkt"
;;  ".git")
