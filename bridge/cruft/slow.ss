#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

;; A quick (but not very general-purpose, alas) way to get partial
;; results from a function that would ordinarily take a long time to
;; run.

(module slow mzscheme
  (require (lib "list.ss" "srfi" "1")
           (lib "trace.ss"))
  (provide partial-map)

  (define (permute l)
    ;; (distribute 3 (list         )) => '()
    ;; (distribute 3 (list 'a      )) => ((3 a    ) (a 3    )                    )
    ;; (distribute 3 (list 'a 'b   )) => ((3 a b  ) (a 3 b  ) (a b 3  )          )
    ;; (distribute 3 (list 'a 'b 'c)) => ((3 a b c) (a 3 b c) (a b 3 c) (a b c 3))
    (define (distribute item l)
      (cond
       ((null? l)
        '())
       ((null? (cdr l))
        (list (list item (car l))
              (list (car l) item)))
       (else
        (cons (cons item l)
              (map (lambda (seq)
                     (cons (car l) seq)) (distribute item (cdr l)))))
       ))

    (cond
     ((null? l)
      '())
     ((null? (cdr l))
      (list l))
     (else
      (apply append
             (partial-map 10 (lambda (seq)
                              (distribute (car l) seq))
                          (permute (cdr l)))))))

  ;; like `map', but might stop before hitting the end of LIST, and
  ;; hence return only partial results.
  (define (partial-map max-seconds-to-run func list)
    (define result '())
    (define (my-map func list)
      (let loop ((l list))
        (if (not (null? l))
            (begin
              (set! result (cons (func (car l))
                                 result))
              (loop (cdr l))))))
    (let ((t (thread (lambda () (my-map func list)))))
      (sync/timeout max-seconds-to-run t)

      ;; if we don't do this, the thread will continue to run and eat
      ;; CPU and memory.
      (kill-thread t)
      )
    (reverse result)
    )
  (display "Hold your horses!  Testing partial-map ...") (flush-output)
  (let ((n 10))
    (printf "There are at least ~a permutations of the first ~a nonnegative integers~%"
            (length (permute (iota n)))
            n))
  (display " done.")
  (newline)  )
