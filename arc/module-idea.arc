;; like the built-in load, but returns the value of the last form in
;; the file.

;; Call it with the name of this very file, and assign the value to,
;; say, "p"; then you can "invoke" "methods" on p like this:

;; arc> ((p 'eat) 'carrots)
;; I spy carrots. Munch munch

(def myload (file)
  (let last nil
    (w/infile f file
      (whilet e (read f)
        (= last (eval e))))
    last))

(prn "I am a side-effect.")

(obj
 talk (fn () (prn "Oink!"))
 move (fn () (prn "Time to fly!"))
 eat  (fn (chow) (prn "I spy " chow ". Munch munch")))
