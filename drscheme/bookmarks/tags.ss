#! /bin/sh
#|
exec mzscheme -qr "$0" ${1+"$@"}
|#

;; Mozilla makes more modifications to the bookmarks file than I'd
;; expect, which makes it scary to merge differing copies.  I have a
;; vague idea that I could parse each copy and do the merge in Lisp ...

(require (lib "htmlprag.ss" "htmlprag"))
(require (lib "list.ss"))

(define (atoms thing)
  (cond
   ((symbol? thing)
    (list thing))
   ((pair? thing)
    (append (atoms (car thing))
            (atoms (cdr thing))))
   (#t
    '())))

(define (sort-symbols syms)
  (mergesort syms (lambda args (apply string<? (map symbol->string args)))))

(define (uniqify sorted-syms)
  (let loop ((thing sorted-syms)
             (result '()))
    (cond 
     ((null? thing)
      (reverse result))
     ((null? result)
      (loop (cdr thing)
            (list (car thing))))
     ((eq? (car thing)
           (car result))
      (loop (cdr thing)
            result))
     (#t
      (loop (cdr thing)
            (cons (car thing)
                  result))))
    ))

(printf "~a~%" 
        (uniqify (sort-symbols
                  (atoms (call-with-input-file "/home/erich/bookmarks.html" (lambda (p) (html->sxml p #t)))))))
