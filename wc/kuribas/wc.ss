#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module wc mzscheme
  (require
   (only (lib "1.ss" "srfi") filter-map append-map iota)
   (only (lib "13.ss" "srfi") string-join string-unfold)
   )

  (define (my-read-line port)
    (let ((chr (read-char port)))
      (if (eof-object? chr) chr
        (string-unfold (lambda (chr)
                         (or (eof-object? chr)
                             (char=? chr #\newline)))
                       values
                       (lambda (_) (read-char port))
                       chr))))

  (define (read-words-tree port word-length)
    (let loop ((tree '()))
      (let ((word (my-read-line port)))
        (if (eof-object? word)
            tree
          (loop
           (if (= (string-length word) word-length)
               (words-tree-add! tree
                                (string->list word)
                                (cons word #t))
             tree))))))

  (define (words-tree-lookup tree word)
    (cond ((null? word) tree)
          ((assoc (car word) tree) =>
           (lambda (pair)
             (words-tree-lookup
              (cdr pair)
              (cdr word))))
          (else #f)))

  (define (words-tree-add! tree word value)
    (cond ((null? word) value)
          ((assoc (car word) tree) =>
           (lambda (pair)
             (set-cdr! pair
                       (words-tree-add!
                        (cdr pair) (cdr word) value))
             tree))
          (else
           (cons (cons (car word)
                       (words-tree-add!
                        '() (cdr word) value))
                 tree))))

  ;; find all words that are equal to word except in
  ;; the letter at position "letter"
  ;; return only words that we haven't found yet, and
  ;; mark them.
  (define (find-matching-words tree word letter)
    (let* ((prefix (string->list (substring word 0 letter)))
           (suffix (string->list (substring word (+ letter 1)
                                            (string-length word))))
           (prefix-tree (words-tree-lookup tree prefix)))
      (if prefix-tree
          (filter-map
           (lambda (pair)
             (let ((match (words-tree-lookup (cdr pair) suffix)))
               (and match (cdr match) ; a match we haven't found yet
                    (begin (set-cdr! match #f)
                           (car match)))))
           prefix-tree)
        '())))

  (define (find-all-matches tree word)
    (append-map (lambda (i)
                  (find-matching-words tree word i))
                (iota (string-length word))))

  (define (complete-chain chains dest)
    (cond ((assoc dest chains) => reverse)
          (else #f)))

  (define (continue-chains tree chains)
    (append-map
     (lambda (chain)
       (map (lambda (match) (cons match chain))
            (find-all-matches tree (car chain))))
     chains))

  (define (mark-word tree word)
    (cond ((words-tree-lookup tree (string->list word))
           => (lambda (cell) (set-cdr! cell #f)))))

  (define (search-chain tree from to)
    (mark-word tree from)
    (let loop ((chains (list (list from))))
      (cond ((null? chains) #f)
            ((complete-chain chains to))
            (else (loop (continue-chains tree chains))))))

  (define (printnl line)
    (display line)
    (newline))

  (define (main2 dict from to)
    (let ((len (string-length from)))
      (if (not (= len (string-length to)))
          (printnl "the words must have the same lenght")
        (let* ((words (call-with-input-file dict
                        (lambda (port)
                          (read-words-tree port len))))
               (chain (search-chain words from to)))
          (if chain
              (printnl (string-join chain " -> "))
            (printnl "no chain found"))))))

  (let ((args (vector->list (current-command-line-arguments))))
    (if (not (= (length args) 3))
        (printnl "Usage: word-chain dict from to")
      (apply main2 args)))

  )
