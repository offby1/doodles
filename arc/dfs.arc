(= nodes* (obj cake '(flour eggs butter milk)
               flour '(wheat)
               eggs  '(chicken)
               butter '(cow)
               milk '(cow)
               chicken '(corn)))

(def adjoin-tail (x xs (o test iso))
  (if (some [test x _] xs)
      xs
      (join xs (list x))))

(mac appendnew (x place . args)
     (w/uniq gx
             (let (binds val setter) (setforms place)
                  `(atwiths ,(+ (list gx x) binds)
                            (,setter (adjoin-tail ,gx ,val ,@args))))))
(let in-progress (table)
  (def deps (item)
    "Find dependencies of ITEM, which is presumably an entry in 'nodes*'"
    (let rv nil
      (when (in-progress item)
        (err 'deps "Circular dependency involving ~a" item)
        )
      (= (in-progress item) t)
      (map [appendnew _ rv]
           (mappend deps (nodes* item)))
      (= (in-progress item) nil)
      (appendnew item rv)
      rv)))


(prn "Here are the dependencies: " (deps 'cake))
(pr "And now, for some error detection:")
(= (nodes* 'chicken)(list 'eggs))
(prn (deps 'cake))
