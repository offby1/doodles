#lang scheme

;; http://programmingpraxis.com/2009/11/27/7-11/

(for* ([a-pennies (in-range 711)]
       [b-pennies (in-range 711)]
       [c-pennies (in-range 711)])
  (let* ([d-pennies (- 711 a-pennies b-pennies c-pennies)]
         [product-dollars (/ (* a-pennies b-pennies c-pennies d-pennies) 100000000)])
    (when (= product-dollars #e7.11)
      (printf "~a + ~a + ~a + ~a~%" a-pennies b-pennies c-pennies d-pennies))))
