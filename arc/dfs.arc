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
  (catch
   (def deps (item)
     ;;(prn item)
     (let rv nil
       (when (in-progress item)
         (throw (tostring (pr "Circular dependency: " item))))
       (= (in-progress item) t)
       (map [appendnew _ rv]
            (mappend deps (nodes* item)))
       (= (in-progress item) nil)
       (appendnew item rv)
       rv))))


(prn "Here are the dependencies: " (deps 'cake))
