;;; -*- lisp -*-

;;(mk:operate-on-system 'memoization 'load)

(defmethod source-file-type ((c cl-source-file) (s (eql (find-system 'anagrams))))
   "l")

(asdf:defsystem anagrams
    :components ((:file "bag"
                        :perform (load-op :before (op c)
                                          (progn (format t "Before loading ~S~%" c)
                                                 (load "snurkly"))))
                 (:file "dict")
                 (:file "anagrams"
                        :in-order-to ((load-op (load-op "bag"))))
                 
                 (:file "profile")
                 ))
