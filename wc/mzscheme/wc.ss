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

  (define-struct agenda-item (trail word))

  (define (bfs start sought)

    (define (ep . args)
      (apply fprintf (cons (current-error-port)
                           args)))

    (define (helper agenda)
      (let ((ai (front-queue agenda)))
        ;;(ep "agenda: ~a~n" (length-queue agenda) )
        (cond
         ((empty-queue? agenda)
          (ep "agenda is empty~n")
          #f)
         ((equal? sought (agenda-item-word ai))
          (ep "Found it~n")
          (agenda-item-trail ai))
         (else
          ;; find all the neighbors which we haven't already done.
          ;; create a new agenda item out of them, and the current agenda.
          (for-each (lambda (n)
                      (insert-queue! agenda
                                     (make-agenda-item (cons (agenda-item-word ai)
                                                             (agenda-item-trail ai))
                                                       n)))
                    (all-neighbors (agenda-item-word ai)))
          (delete-queue! agenda)
          (helper agenda )))))

    ;;(trace helper)
    (let ((rv (helper (make-queue (list (make-agenda-item '() start))))))
      (and rv (reverse (cons sought rv)))))

  ;;(display (bfs "foo" "bar"))
  (display (bfs "giant" "raven"))
  (newline)
  
  )
