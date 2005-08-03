#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module wc mzscheme
  
  (require
   (only (lib "1.ss" "srfi") filter every)
   (planet "test.ss"    ("schematics" "schemeunit.plt" 1))
   (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
   (planet "util.ss"    ("schematics" "schemeunit.plt" 1))
   "network.ss"
   "olvs.ss"
   "dict.ss"
   "persist.ss"
   )

  (define-persistent *network* "network.dat"
    (let ((rv (new-network)))
      (hash-table-for-each *word-hash*
                           (lambda (word ignored)
                             (let ((word-node (or (get-node-by-name rv word)
                                                  (new-node word 'dummy-data))))
                               (for-each
                                (lambda (neighbor)
                                  (let ((neighbor-node (or (get-node-by-name rv neighbor)
                                                           (new-node neighbor 'dummy-data))))
                                    (link-nodes! word-node
                                                 neighbor-node)
                                    (put-node! rv neighbor-node))
                                  )
                                (filter (lambda (w)
                                          (hash-table-get *word-hash* w (lambda () #f)))
                                        (olvs word)))
                               (put-node! rv word-node))))
      rv))

  #;(define chain (lambda (from to) (let ((from-node (get-node *network* from)) (to-node   (get-node *network* to))) (and (from-node to-node) (bfs *network* from-node to-node)))))

  ;(printf "~a~n" (chain "sheboygan" "Paris"))

  (let ((network-as-alist (map (lambda (p)
                                 (cons (car p)
                                       (get-neighbor-names (cdr p))))
                               (network->list *network*))))
    ;; alas, I've found #f in my data, and don't know where it's coming from.
    (for-each (lambda (seq)
                (when (not (every string? seq))
                  (error "Found non-string in seq" seq)
                  ))
              network-as-alist)
    (for-each (lambda (thing)
                (printf "~s~n" thing))
              (filter 
               (lambda (thing)
                 (< 1 (length thing)))
               network-as-alist)))

  (test/text-ui
   (make-test-suite
    "I never know what to name these things."
    (make-test-case
     "Duh.  Drooool."

     (assert = 0 0)
     )
    ))
  )