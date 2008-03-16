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

(let p (myload "module.arc")
  (p!talk))
