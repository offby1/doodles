(module score mzscheme
  (provide score-from-declarer-tricks)
  (define (score-from-declarer-tricks tricks auction)
    (equal-hash-code (list tricks auction))))