#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui sandboxes-tests 'verbose))"
|#
(module sandboxes mzscheme
(require (lib "sandbox.ss")
         (lib "trace.ss")
         (only (lib "list.ss") sort)
         (only (lib "1.ss" "srfi") iota)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only (planet "assert.ss" ("offby1" "offby1.plt")) assert)
         "test-utils.ss"
         "vprintf.ss")

(define-struct sandbox (evaluator last-used-time) #f)
(define (public-make-sandbox)
  (let ((sandbox-op (open-output-string))
        (sandbox-ep (open-output-string)))
    (make-sandbox
     (parameterize ((sandbox-output       sandbox-op)
                    (sandbox-error-output sandbox-ep)
                    (sandbox-eval-limits '(2 20)))

       (make-evaluator 'mzscheme '() '()))

     0)))

(define (sandbox-eval sb string)
  (set-sandbox-last-used-time! sb (current-milliseconds))
  ((sandbox-evaluator sb) string))

(define (get-sandbox-by-name name)
  ;; if necessary, "evict" the least-recently-used evaluator.

  (when (and (not (hash-table-get *sandboxes-by-nick* name #f))
             (= (hash-table-count *sandboxes-by-nick*)
                (*max-sandboxes*)))
    (let ((name-of-lru
           (cdar
            (sort
             (hash-table-map
              *sandboxes-by-nick*
              (lambda (k v)
                (cons (sandbox-last-used-time v) k)))
             (lambda (p1 p2)
               (< (car p1)
                  (car p2)))))))

      (assert (not (equal? name-of-lru name)))

      (vtprintf "~s => name-of-lru: ~s~%"
                *sandboxes-by-nick*
                name-of-lru)

      (vtprintf "Evicting ~s => ~s~%"
                name-of-lru (hash-table-get *sandboxes-by-nick* name-of-lru))
      (hash-table-remove! *sandboxes-by-nick* name-of-lru)))

  (let ((rv (hash-table-get *sandboxes-by-nick* name (lambda () (public-make-sandbox)))))
    (vtprintf "Just put! an evaluator for ~s~%" name)
    (hash-table-put! *sandboxes-by-nick* name rv)
    (hash-table-for-each
     *sandboxes-by-nick*
     (lambda (k v)
       (vtprintf "~s => ~s~%"
                 k v)))
    rv))
;(trace get-sandbox-by-name)

(define *sandboxes-by-nick* (make-hash-table 'equal))

(define *max-sandboxes* (make-parameter 3))


(print-hash-table #t)
(define sandboxes-tests

  (test-suite
   "sandboxes"
   (test-case
    "simple get"
    (let ((s  (get-sandbox-by-name "charlie")))
      (check-pred sandbox? s)
      (check-equal? (sandbox-eval s "3") 3)))

   (let ((charlies-sandbox (get-sandbox-by-name "charlie"))
         (ednas-sandbox    (get-sandbox-by-name "edna")))
     (test-false
      "keeps sandboxes distinct, by name"
      (eq? charlies-sandbox ednas-sandbox))
     (test-case
      "remembers state"
      (sandbox-eval charlies-sandbox "(define x 99)")
      (let ((this-better-still-be-charlies (get-sandbox-by-name "charlie")))
        (check-equal? (sandbox-eval this-better-still-be-charlies
                       "x")
                      99))
      (check-exn
       exn:fail?
       (lambda () (sandbox-eval ednas-sandbox))
       "edna's sandbox didn't gack when I referenced 'x' -- even though we never defined it.")))

   (test-case
    "won't store too many"
    (before
     (set! *sandboxes-by-nick* (make-hash-table 'equal))
     (let* ((names (iota (* 2 (*max-sandboxes*))))
            (boxes (map get-sandbox-by-name names)))
       (check-equal? (hash-table-count *sandboxes-by-nick*)
                     (*max-sandboxes*)))))
   (test-case
    "evicts oldest"
    (before
     (set! *sandboxes-by-nick* (make-hash-table 'equal))
     ;; now I have to decide precisely when old sandboxes get
     ;; evicted.  Is it when we call get-sandbox-by-name?  And I have
     ;; to decide when the timestamp updates -- again, when we call
     ;; get-sandbox-by-name?  Or when we run its evaluator?  There
     ;; might not be any practical difference.
     (vtprintf "~a~%" (make-string 80 #\-))
     (parameterize ((*max-sandboxes* 2))
       (sandbox-eval (get-sandbox-by-name "old"   ) "(define x 'old   )")
       (sleep 1/10)
       (sandbox-eval (get-sandbox-by-name "young" ) "(define x 'young )")
       (sleep 1/10)
       (sandbox-eval (get-sandbox-by-name "newest") "(define x 'newest)")

       (check-equal? (sandbox-eval (get-sandbox-by-name "young")
                      "x")
                     'young)
       (check-equal? (sandbox-eval (get-sandbox-by-name "newest")
                      "x")
                     'newest)
       (check-exn exn:fail? (lambda () (sandbox-eval (get-sandbox-by-name "old")
                      "x"))))))))

(provide *max-sandboxes*
         get-sandbox-by-name
         sandbox-eval
         sandboxes-tests
         (rename public-make-sandbox make-sandbox))
)
