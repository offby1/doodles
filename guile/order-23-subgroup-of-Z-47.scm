(sort
 (map (lambda (n)
        (modulo (expt 17 n) 47)) (iota 23))
 <)
