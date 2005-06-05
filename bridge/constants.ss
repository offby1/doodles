(module constants mzscheme
  (require (lib "list.ss" "srfi" "1"))
  (provide 
   *deck-size*
   *denominations*
   *ranks*
   *seats*
   *suits*
   nth-successor
   seat->number
   )
  (define *seats* '(north east south west))
  (define (seat->number s)
    (list-index (lambda (x)
                  (eq? x s))
                *seats*))
  ;; (nth-successor 'north 0) => 'north
  ;; (nth-successor 'north 1) => 'east
  ;; (nth-successor 'north 102) => 'south
  ;; (nth-successor 'north 203) => 'west
  (define (nth-successor seat n)
    
    (list-ref *seats* (modulo (+ (seat->number seat) n) (length *seats*))))
  (define *suits* '(clubs diamonds hearts spades))
  (define *denominations* (append *suits* '(notrump)))
  (define *deck-size* 52)
  (define *ranks* (iota 13 2))  )
