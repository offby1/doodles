#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module wc mzscheme

  (require
   (lib "trace.ss")
   (planet "test.ss"    ("schematics" "schemeunit.plt" 1))
   (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
   (planet "util.ss"    ("schematics" "schemeunit.plt" 1))

   "dict.ss")

  (define (bfs start sought)

    ;; probably slow
    (define (adjoin agenda item)
      (if (member item agenda)
          agenda
        (append agenda (list item))))

    (define (helper agenda minutes)
      (fprintf (current-error-port) "Helper: agenda: ~a; minutes: ~a~n"
               (length agenda)
               (length minutes))
      (cond
       ((null? agenda) #f)
       ((equal? (car agenda) sought) minutes)
       (else
        (for-each (lambda (n)(set! agenda (adjoin agenda n)))
                  (all-neighbors (car agenda)))
        (helper (cdr agenda)
                (cons (car agenda) minutes)))))
    ;;(trace helper)
    (helper (cons start (all-neighbors start)) '()))

  (display (bfs "foo" "bar"))
  (newline)
  )
