(module constants mzscheme
  (require (lib "list.ss" "srfi" "1"))
  (provide *seats*
           seat->number
           *suits*
           *denominations*
           *deck-size*
           *ranks*)
  (define *seats* '(north east south west))
  (define (seat->number s)
    (list-index (lambda (x)
                  (eq? x s))
                *seats*))
  (define *suits* '(clubs diamonds hearts spades))
  (define *denominations* (append *suits* '(notrump)))
  (define *deck-size* 52)
  (define *ranks* (iota 13 2))  )
