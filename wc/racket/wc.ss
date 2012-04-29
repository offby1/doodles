#lang scheme

(require
 (planet offby1/offby1/bfs)
 "dict.ss")

(define (display-result chain)
  (when chain
    (printf "~a: ~a~n"
            (length chain)
            (string-join chain " -> "))))

(define (go word-pair)
  (apply bfs  (append word-pair (list string=?  all-neighbors))))

(let ((args (vector->list (current-command-line-arguments))))
  (if (= 2 (length args))
      (display-result (go args) #t)
      (let loop ()
        (display-result (go (random-word-pair 6)))
        (loop))))

;;; For further reading:

;; http://www.policyalmanac.org/games/aStarTutorial.htm
