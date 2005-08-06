#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module wc mzscheme

  (require
   (lib "trace.ss")
   (only (lib "13.ss" "srfi") string-join)
   "set.ss"
   "q.ss"
   "dict.ss")

  ;; breadth-first search requires an "agenda", which is a queue of
  ;; work to do.  But in case we find a solution, we want to know how
  ;; we got there; the trail holds the "how we got there".
  
  ;; I've seen a number of articles about searching that use the term
  ;; "open list" to refer to what I am calling "agenda".
  
  (define-struct agenda-item (trail word))

  (define (bfs start sought)

    ;; words we've already considered.  If we didn't keep track, we'd
    ;; loop endlessly.
    (define seen (make-set))

    (define (helper agenda)
      (if (empty-queue? agenda)
          #f
        (let* ((ai (front-queue agenda))
               (w (agenda-item-word ai))
               (trail (agenda-item-trail ai)))

          (cond
           ((equal? sought w) trail)
           (else
            ;; find all the neighbors which we haven't already done.
            ;; create a new agenda item out of them, and the current
            ;; agenda.
            (for-each (lambda (n)
                        (when (not (is-present? n seen) )
                          (add! n seen)
                          (insert-queue! agenda
                                         (make-agenda-item (cons w trail)
                                                           n))))
                      (all-neighbors w))
            (delete-queue! agenda)
            (helper agenda))))))

    (let ((rv (helper (make-queue (list (make-agenda-item '() start))))))
      (and rv (reverse (cons sought rv)))))

  (define (display-result chain say-bummer?)
    (cond
     (chain => (lambda (result)
                 (printf "~a: ~a~n"
                         (length result)
                         (string-join result " -> "))))
     (else
      (when say-bummer?
        (display "Bummer.  No chain.")
        (newline)))))

  (let ((args (vector->list (current-command-line-arguments))))
    (if (= 2 (length args))
        (display-result (apply bfs args) #t)
      (let loop ()
        (set! args  (random-word-pair 6))
        (display-result (apply bfs args) #f)
        (loop)))))

;;; For further reading:

;; http://www.policyalmanac.org/games/aStarTutorial.htm
