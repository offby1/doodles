#lang scheme

;; http://programmingpraxis.com/2009/11/27/7-11/

(for*/first ([a-pennies (in-range 711)]
             [b-pennies (in-range a-pennies)]
             [c-pennies (in-range b-pennies)]
             [d-pennies (in-value (- 711 a-pennies b-pennies c-pennies))]
             #:when (=  (/ (* a-pennies b-pennies c-pennies d-pennies) 100000000) #e7.11))
            (printf "~a + ~a + ~a + ~a~%" a-pennies b-pennies c-pennies d-pennies)
            )
