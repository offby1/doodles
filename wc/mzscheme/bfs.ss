#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module bfs mzscheme

  (require
   (only (lib "1.ss" "srfi") remove)
   (only "set.ss" make-set is-present? add!)
   "q.ss"
   )
  (provide bfs)
  
  (define (ep . args)
    (apply fprintf (cons (current-error-port)
                         args)))

  (define (pop-queue! q)
    (begin0
      (front-queue q)
      (delete-queue! q)))
  
  (define-struct agenda-item (trail word))
  
  (define (bfs start-node goal-node nodes-equal? node-neighbors)

    (define *already-seen* (make-set))
  
    (define (already-seen? thing)
      (is-present? thing *already-seen*))
  
    (define (been-there-done-that! thing)
      (add! thing *already-seen*))

    (define *the-queue* (make-queue (list (make-agenda-item '() start-node))))

    (define (loop)
      (if (empty-queue? *the-queue*) #f
        (let* ((ai (front-queue *the-queue*))
               (w (agenda-item-word ai))
               (trail (agenda-item-trail ai)))

          (cond
           ((nodes-equal? goal-node w) trail)
           (else
            (for-each (lambda (n)
                        (been-there-done-that! n)
                        (insert-queue! *the-queue*
                                       (make-agenda-item (cons w trail) n)))
                      (remove already-seen? (node-neighbors w)))
            (delete-queue! *the-queue*)
            (loop))))))

    (let ((rv (loop)))
      (and rv (reverse (cons goal-node rv))))

    )

;;; A fake network, for testing.

  (define nodes-equal? string=?)
  (define (node-neighbors n)
    (cond
     ((nodes-equal? n "start") (list "a" "b"))
     ((nodes-equal? n "a") (list "start" "c"))
     ((nodes-equal? n "b") (list "start" "c" "e"))
     ((nodes-equal? n "c") (list "a" "b" "e"))
     ((nodes-equal? n "e") (list "b" "c" "d" "f"))
     ((nodes-equal? n "d") (list "e"))
     ((nodes-equal? n "f") (list "e" "goal"))
     ((nodes-equal? n "goal") (list "f"))
     ((nodes-equal? n "outlier") (list))
     ((nodes-equal? n "cycle-a") (list "cycle-b"))
     ((nodes-equal? n "cycle-b") (list "cycle-a"))
     (else
      (error 'node-neighbors "Unknown node ~s" n))))

  (printf "This should succeed: ~s~n" (bfs "start" "goal" nodes-equal? node-neighbors))
  (printf "This should fail: ~s~n"    (bfs "outlier" "goal" nodes-equal? node-neighbors))
  (printf "This too: ~s~n"            (bfs "cycle-a" "goal" nodes-equal? node-neighbors))
  )