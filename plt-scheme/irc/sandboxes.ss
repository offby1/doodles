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

(define (get-sandbox-by-name name)
  (let ((rv (hash-table-get *sandboxes-by-nick* name (lambda () (public-make-sandbox)))))
    (hash-table-put! *sandboxes-by-nick* name rv)
    rv))

(define *sandboxes-by-nick* (make-hash-table 'equal))

(define sandboxes-tests

  (test-suite
   "sandboxes"
   (test-case
    "simple get"
    (let ((s  (get-sandbox-by-name "charlie")))
      (check-pred sandbox? s)
      (check-equal? ((sandbox-evaluator s) "3") 3)))

   (let ((charlies-sandbox (get-sandbox-by-name "charlie"))
         (ednas-sandbox    (get-sandbox-by-name "edna")))
     (test-false
      "keeps sandboxes distinct, by name"
      (eq? charlies-sandbox ednas-sandbox))
     (test-case
      "remembers state"
      ((sandbox-evaluator charlies-sandbox) "(define x 99)")
      (let ((this-better-still-be-charlies (get-sandbox-by-name "charlie")))
        (check-equal? ((sandbox-evaluator this-better-still-be-charlies)
                       "x")
                      99))
      (check-exn
       exn:fail?
       (lambda () ((sandbox-evaluator ednas-sandbox "x")))
       "edna's sandbox didn't gack when I referenced 'x' -- even though we never defined it."))
     )
   ))

;; (lambda ()
;;   (and (< (hash-table-count *sandboxes-by-nick*) 10)
;;        (parameterize ((sandbox-output       sandbox-op)
;;                       (sandbox-error-output sandbox-ep)
;;                       (sandbox-eval-limits '(2 200)))
;;          (make-evaluator 'mzscheme '() '()))))

(provide (all-defined))
)
