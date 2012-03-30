#lang racket

(define (funny-dirlist d)
  (map
   (lambda (short)
     (cons short (build-path d short)))
   (directory-list d)))

(define (executable? candidate)
  (and (file-exists? candidate)
       (memq 'execute (file-or-directory-permissions candidate))))

(define (executables-in-directory d)
  (for/fold ([result '()])
      ([pair (funny-dirlist d)])
      (if (executable? (cdr pair))
          (cons (car pair )
                result)
          result)))

(let ([basename-to-dirs (make-hash)])
  (for ([d (map build-path (regexp-split #rx":" (getenv "PATH")))])
    (when (directory-exists? d)
      (for ([exe (executables-in-directory d)])
        (hash-update! basename-to-dirs exe (lambda (old)
                                             (set-add old d))
                      (set d)))))

  (for ([(basename dirs) (in-hash basename-to-dirs)])
    (when (< 1 (set-count dirs))
      (printf "~a is in ~a~%" basename (string-join (set-map dirs (curry format "~a")) ", ")))))
