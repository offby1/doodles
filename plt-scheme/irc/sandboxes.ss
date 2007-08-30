#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui sandboxes-tests 'verbose))"
|#
(module sandboxes mzscheme
(require (lib "sandbox.ss")
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "test-utils.ss")

(define-struct sandbox (evaluator last-used-time) #f)
(define (public-make-sandbox)
  (let ((sandbox-op (open-output-string))
        (sandbox-ep (open-output-string)))
    (make-sandbox
     (parameterize ((sandbox-output       sandbox-op)
                    (sandbox-error-output sandbox-ep)
                    (sandbox-eval-limits '(2 20)))

       (make-evaluator 'mzscheme '() '()))

     #f)))

(define (get-sandbox-by-name . args)
  (public-make-sandbox))

(define *sandboxes-by-nick* #f)

(define sandboxes-tests

  (test-suite
   "sandboxes"
   (test-case
    "simple get"
    (let ((s  (get-sandbox-by-name *sandboxes-by-nick* "charlie")))
      (check-pred sandbox? s)
      (check-equal? ((sandbox-evaluator s) "3") 3)))
   ))

;; (lambda ()
;;   (and (< (hash-table-count *sandboxes-by-nick*) 10)
;;        (parameterize ((sandbox-output       sandbox-op)
;;                       (sandbox-error-output sandbox-ep)
;;                       (sandbox-eval-limits '(2 200)))
;;          (make-evaluator 'mzscheme '() '()))))

(provide (all-defined))
)
