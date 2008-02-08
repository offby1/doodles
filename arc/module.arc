(prn "I am a side-effect.")
(obj
 talk (fn () (prn "Oink!"))
 move (fn () (prn "Time to fly!"))
 eat  (fn (chow) (prn "I spy " chow ". Munch munch")))
