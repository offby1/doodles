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

(def w/prefix (p sym)
  (coerce
   (+
    (coerce p 'string)
    "."
    (coerce sym 'string))
   'sym))

;; (with-clause 'm (obj a 1 b 2)) => (m.a 1 m.b 2)

(def with-clause (dotname table)
  (mappend
   [ cons (w/prefix dotname (car _)) (cdr _) ]
   (tablist table)))

(hoo m 
     (prn "m.b is " m.b)
     (prn "Oh, and m.a is " m.a))

; =>

(w/uniq t 
 `(let ,t (obj a 1 b 2)
    (with ;;,(with-clause 'm t)
      (m.a (,t 'a) m.b (,t 'b))
      (prn "m.b is " m.b)
      (prn "Oh, and m.a is " m.a))))
