;; given an ID, look through the database for the section that has
;; that ID, and return it.  If the database were an alist, we'd just
;; call `assq'.

(define (lookup symbol)
  (let loop ((sections (gedcom-sections gdb)))
    (cond
     ((null? sections)
      #f)
     ((eq? symbol (kdp-keyword (section-kdp (car sections))))
      (car sections))
     (#t
      (loop (cdr sections))))))
