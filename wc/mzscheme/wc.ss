#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module wc mzscheme

  (require
   (lib "trace.ss")
   (only (lib "1.ss" "srfi") any append-map remove)
   "set.ss"
   "q.ss"
   "dict.ss")

  ;; breadth-first search requires an "agenda", which is a queue of
  ;; work to do.  But in case we find a solution, we want to know how
  ;; we got there; the trail holds the "how we got there".
  (define-struct agenda-item (trail word))

  (define (bfs start sought)

    (define seen (set))
    
    (define (ep . args)
      (apply fprintf (cons (current-error-port)
                           args)))

    (define (helper agenda)
      (ep "agenda: ~a~n" (length-queue agenda) )
      (if (empty-queue? agenda)
          (begin
            (ep "agenda is empty~n")
            #f)
        (let* ((ai (front-queue agenda))
               (w (agenda-item-word ai))
               (trail (agenda-item-trail ai)))
            
          (cond
           ((equal? sought (agenda-item-word ai))
            (ep "Found it~n")
            (agenda-item-trail ai))
           (else
            ;; find all the neighbors which we haven't already done.
            ;; create a new agenda item out of them, and the current agenda.
            (for-each (lambda (n)
                        (when (not (is-present? n seen) )
                          (fprintf (current-error-port) "n is ~s~n" n)
                          (add! n seen)
                          (insert-queue! agenda
                                         (make-agenda-item (cons w trail)
                                                           n))))
                      (all-neighbors w))
            (delete-queue! agenda)
            (helper agenda )))))
      )

    ;;(trace helper)
    (let ((rv (helper (make-queue (list (make-agenda-item '() start))))))
      (and rv (reverse (cons sought rv)))))

  ;;(display (bfs "foo" "bar"))
  (display (bfs "fuck" "shit"))
  ;;(display (bfs "giant" "raven"))
  (newline)
  
  )
