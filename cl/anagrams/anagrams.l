(defun anagrams-internal (bag dict)
  ;;(format t "~A~&" (bag-to-string bag))
  (let ((rv '()))
    (maplist
     #'(lambda (subdict)
         
         (let* ((word (car subdict))
                (smaller-bag (bag-reduce bag word)))
           
           (when smaller-bag
             
             (if (bag-emptyp smaller-bag)
                 (setq rv (nconc rv (list (list word))))
               (let ((more-anagrams (anagrams-internal smaller-bag subdict)))
                 
                 (if more-anagrams
                     (setq rv (nconc rv (mapcar #'(lambda (l)
                                                    (cons word l)) more-anagrams)))))))))
     dict)
    rv))

(defun anagrams (bag)
  (anagrams-internal bag *dict*))

(let ((dict (make-dictionary (list "hey" "you"))))
  (assert (= 1 (length (anagrams-internal (bag "heyyou") dict)))))

(let ((dict (make-dictionary (list))))
    (assert (null (anagrams-internal (bag "I don't expect any!") dict))))

(let ((dict (make-dictionary (list "hey" "you"))))
  (assert (null (anagrams-internal (bag "") dict))))

(let ((dict (make-dictionary (list "ho" "uy" "ye" "he" "hey" "you"))))
  (anagrams-internal (bag "you hey") dict))
