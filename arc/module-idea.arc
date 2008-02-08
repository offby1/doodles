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

(let with-module
    (fn (name table . body)
        (cons 'with
              (cons
               (mappend
                [ let (sym proc) _ 
                      (when (no (isa sym 'sym)) (err sym "is not a symbol"))
                      (list (coerce (+ (coerce name 'string)
                                       "." 
                                       (coerce sym 'string)) 
                                    'sym) 
                            proc)] 
                (tablist table))
               body)))


  (with-module 'pig (obj
                     talk (fn () (prn "Oink!"))
                     move (fn () (prn "Time to fly!"))
                     eat  (fn (chow) (prn "I spy " chow ". Munch munch")))
               '(prn "One")
               '(pig.talk "Two")))