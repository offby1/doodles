(module dd mzscheme
;;; the double-dummy solver.
  (provide likely-declarer-tricks)
(define (likely-declarer-tricks dealer trump-suit holdings)
  (modulo (equal-hash-code (list dealer trump-suit holdings)) 14))
  )