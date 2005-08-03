#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module wc mzscheme
  
  (require
   (only (lib "1.ss" "srfi") filter)
   (only (lib "13.ss" "srfi") string-downcase)
   (planet "test.ss"    ("schematics" "schemeunit.plt" 1))
   (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
   (planet "util.ss"    ("schematics" "schemeunit.plt" 1))
   (lib "file.ss")
   "network.ss"
   "olvs.ss")
  
  (define *dictionary-file-name*
    (let ((t (get-preference 'anagrams-dictionary-file-name)))
      (or (and t (bytes->path t))
          (bytes->path "/usr/share/dict/words"))))

  (define (snarf-dict)
    (define (word-acceptable? w)
      (positive? (string-length w)))
    (with-input-from-file *dictionary-file-name*
      (lambda ()
        (let loop ((word  (read-line))
                   (lines-read 0))
          (when (and
                 (< lines-read 20)
                 (not (eof-object? word)))
            (set! word (string-downcase word))
            (when (word-acceptable? word)
                 (hash-table-put! *word-list* word #t))
            (loop (read-line)
                  (+ 1 lines-read)))))))

  (define *word-list* (make-hash-table 'equal))
  (if #f (snarf-dict)
    (begin
      (hash-table-put! *word-list* "turd" #t)
      (hash-table-put! *word-list* "third" #t)
      (hash-table-put! *word-list* "bird" #t)
      (hash-table-put! *word-list* "tird" #t)
      ))

  (define (build-network)
    (let ((rv (new-network)))
      (hash-table-for-each *word-list*
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
                                          (hash-table-get *word-list* w (lambda () #f)))
                                        (olvs word)))
                               (put-node! rv word-node))))
      rv))

  #;(define chain
    (let ((n (build-network)))
      (lambda (from to)
        (let ((from-node (get-node n from))
              (to-node   (get-node n to)))
          (and (from-node to-node)
               (bfs n from-node to-node)))
        )))

  #;(printf "~a~n" (chain "sheboygan" "Paris"))
  (test/text-ui
   (make-test-suite
    "I never know what to name these things."
    (make-test-case
     "Duh.  Drooool."
     (for-each (lambda (thing)
                 (printf "~s~n" thing))
               (map (lambda (p)
                      (cons (car p)
                             (get-neighbor-names (cdr p))))
                    (network->list (build-network))))
     (assert = 0 0)
     )
    ))
  )