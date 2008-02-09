;; like the built-in load, but returns the value of the last form in
;; the file.

(def myload (file)
  (let last nil
    (w/infile f file
      (whilet e (read f)
        (= last (eval e))))
    last))

(let p (myload "module.arc")
  ((p 'eat) 'carrots))

(w/mod p "module.arc"
   (p.eat 'carrots))

(w/mod p (obj eat (fn (chow) (prn "I like to eat " chow)))
   (p.eat 'junk-food))
; =>

(def ensure-syms (seq)
  (map [if (no (isa _ 'sym))
           (err 'w/mod (tostring (pr _ " is not a symbol")))
           _]
       seq))

(def w/mod (namesym tablename . body-exprs)
  (withs (keys (prn (ensure-syms (keys tablename)))
          newsyms (prn (map
                   [coerce (+ (coerce namesym 'string)
                              "." 
                              (coerce _ 'string)) 
                           'sym]
                   keys))
          
          )
          (list 'with
                (mappend (fn (new old) (list new (tablename old))) newsyms keys)
                '(+ 1 2))
         ))

(let some-table (obj
                 eat (fn (chow) (prn "I like to eat " chow))
                 fly (fn () (prn "Pigs can't fly!!"))
                 )
  (w/mod 'p
         some-table
         '(p.eat 'junk-food)
         '(p.eat 'healthy)))