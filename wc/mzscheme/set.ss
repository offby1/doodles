#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module set mzscheme
  (provide
   set
   is-present?
   add!
   )

  (define (set . items)
    (let ((rv (make-hash-table 'equal)))
      (for-each (lambda (item) (add! item rv))
                items)
      rv))

  (define (is-present? item set)
    (hash-table-get set item (lambda () #f)))

  (define (add! item set)
    (hash-table-put! set item #t)
    set)


  )