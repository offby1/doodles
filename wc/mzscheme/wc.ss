#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module wc mzscheme

  (require
   (lib "trace.ss")
   "set.ss"
   "q.ss"
   "dict.ss")

  (define (bfs start sought)

    (define (ep . args)
      (apply fprintf (cons (current-error-port)
                           args)))

    (define (helper agenda done call-stack)
      (ep "agenda: ~a; done: ~a; call-stack: ~a~n" (length-queue agenda) (count done) (length call-stack))
      (cond
       ((empty-queue? agenda)
        (ep "agenda is empty~n")
        #f)
       ((is-present? (front-queue agenda) done)
        (ep "Seen ~s already ... " (front-queue agenda))
        (delete-queue! agenda)
        (ep "trying again with ~s~n" (front-queue agenda))
        (helper agenda done   call-stack))
       ((equal? (front-queue agenda) sought)
        (ep "Found it~n")
        call-stack)
       (else
        (for-each (lambda (n)
                    (when (not (is-on-queue? n agenda))
                      (insert-queue! agenda n)))
                  (all-neighbors (front-queue agenda)))
        (helper agenda
                (add! (front-queue agenda) done)
                (cons (front-queue agenda) call-stack)))))

    ;;(trace helper)
    (helper (make-queue (list start))
            (set)
            (list)))

  (display (bfs "foo" "bar"))
  (newline)
  )
