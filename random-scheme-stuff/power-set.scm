;; () => ()
;; (foo) => ((foo) ())
;; (foo bar) => ((foo bar) (foo) (bar) ())
;; (f b z) => ((f b z) (f b) (f z) (b z) (f) (b) (z) ())

;; for mzscheme v300

(require (lib "1.ss" "srfi"))

(define (combine sym ps-list)
  (append
   (map (lambda (sym-seq)
          (cons sym sym-seq))
        ps-list)
   ps-list))

(define (power-set seq)
  (if (null? seq)
      '(())
    (combine (car seq) (power-set (cdr seq))))
  )