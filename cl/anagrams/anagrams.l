(defun anagrams-internal (bag dict level)
  ;;(format t "~A~&" (bag-to-string bag))
  (when (zerop level) (format t "Incoming dictionary is ~A words ..." (length dict)))

  ;; This isn't necessary for correctness, but it makes a HUGE
  ;; difference in speed.
  (setf dict (mapcan #'(lambda (word) (and (bag-reduce bag word)
                                           (list word)))
                     dict))


  (when (zerop level) (format t "reduced to ~A words.~%" (length dict)))
  (let ((rv '()))
    (maplist
     #'(lambda (subdict)
         (when (zerop level) (format t ".")(finish-output))
         (let* ((word (car subdict))
                (smaller-bag (bag-reduce bag word)))
           
           (when smaller-bag
             
             (if (bag-emptyp smaller-bag)
                 (progn
                   
                   (setq rv (nconc rv (list (list word)))))
               (let ((more-anagrams (anagrams-internal smaller-bag subdict (+ 1 level))))
                 
                 (when more-anagrams
                   (let ((with-new-word (mapcar #'(lambda (l)
                                                    (cons word l)) more-anagrams)))
                     (when (zerop level)
                       (format t "~&~A" with-new-word))
                     (setq rv (nconc rv with-new-word)))))))))
     dict)
    rv))

(defun anagrams (bag)
  (anagrams-internal bag *dict* 0))

(let ((dict (make-dictionary (list "hey" "you"))))
  (assert (= 1 (length (anagrams-internal (bag "heyyou") dict 0)))))

(let ((dict (make-dictionary (list))))
    (assert (null (anagrams-internal (bag "I don't expect any!") dict 0))))

(let ((dict (make-dictionary (list "hey" "you"))))
  (assert (null (anagrams-internal (bag "") dict 0))))

(let ((dict (make-dictionary (list "ho" "uy" "ye" "he" "hey" "you"))))
  (anagrams-internal (bag "you hey") dict 0))
