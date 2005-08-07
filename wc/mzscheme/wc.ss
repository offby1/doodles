#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module wc mzscheme

  (require
   (lib "trace.ss")
   (only (lib "13.ss" "srfi") string-join)
   "bfs.ss"
   "set.ss"
   "q.ss"
   "dict.ss")
  
  (define (display-result chain say-bummer?)
    (cond
     (chain => (lambda (chain)
                 (printf "~a: ~a~n"
                         (length chain)
                         (string-join chain " -> "))))
     (else
      (when say-bummer?
        (display "Bummer.  No chain.")
        (newline)))))

  (define (go word-pair)
    (apply bfs  (append word-pair (list string=?  all-neighbors))))

  (let ((ccla (vector->list (current-command-line-arguments))))
    (if (= 2 (length ccla))
        (display-result (go ccla) #t)
      (let loop ()
        (display-result (go (random-word-pair 6)) #f)
        (loop)))))

;;; For further reading:

;; http://www.policyalmanac.org/games/aStarTutorial.htm
