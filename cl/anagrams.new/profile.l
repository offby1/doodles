(load "bag.l")
(load "dict.l")
(load "anagrams.l")

(setq *dict* (make-dictionary 
                (list "sting" "me" "anywhere" )
                ;;"/usr/share/dict/words"
                ))

(ext:space (setq result (anagrams (bag "ernest hemingway"))))
(format t "~&~A~%" result)
(quit)