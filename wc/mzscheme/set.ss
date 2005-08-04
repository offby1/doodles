#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module set mzscheme
  (provide
   set
   is-present?
   add!
   remove!
   count
   )

  (define (set . items)
    (let ((rv (make-hash-table)))
      (for-each (lambda (item) (add! item rv))
                items)
      rv))

  (define (is-present? item set)
    (hash-table-get set item (lambda () #f)))

  (define (add! item set)
    (hash-table-put! set item #t)
    set)

  (define (remove! item set)
    (when (not (is-present? item set))
      (error 'remove "No item ~s in set" item))
    (hash-table-remove! set item)
    set)

  (define count hash-table-count)

  )