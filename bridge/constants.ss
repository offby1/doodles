(module constants mzscheme
  (provide *seats* *suits* *denominations*)
  (define *seats* '(north east south west))
  (define *suits* '(clubs diamonds hearts spades))
  (define *denominations* (append *suits* '(notrump)))
  )