(defun anagrams (bag)
  (let ((rv '()))
    (map-dict
     #'(lambda (word)
         
         (let ((smaller-bag (bag-reduce bag word)))
           
           (when smaller-bag
             
             (if (bag-emptyp smaller-bag)
                 (setq rv (nconc rv (list (list word))))
               (let ((more-anagrams (anagrams smaller-bag)))
                 
                 (if more-anagrams
                     (setq rv (nconc rv (mapcar #'(lambda (l)
                                                    (cons word l)) more-anagrams)))))))))
     *dict*)
    rv))

(load "bag")
(load "dict")

(defparameter *dict* (make-dictionary (list "hey" "you")))
(assert (= 1 (length (anagrams (bag "heyyou")))))
(let ((*dict* (make-dictionary (list))))
  (assert (null (anagrams (bag "I don't expect any!")))))

(let ((*dict* (make-dictionary (list "hey" "you"))))
  (assert (null (anagrams (bag "")))))

(let ((*dict* (make-dictionary (list "ho" "uy" "ye" "he" "hey" "you"))))
  (anagrams (bag "you hey")))
