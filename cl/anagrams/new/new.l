(defun anagrams (bag)
  (let ((rv '()))
    (maphash
     #'(lambda (word ignored)
         (let ((smaller-bag (bag-reduce bag word)))
           (when smaller-bag
             (if (bag-emptyp smaller-bag)
               (setq rv (nconc rv (list word)))
               (let ((more-anagrams (anagrams smaller-bag)))
                 (when more-anagrams
                   (setq rv (mapcar #'(lambda (l)
                                        (cons word l)) more-anagrams)))
                 ))))
         )
     *dict*)
    rv))

(trace anagrams)