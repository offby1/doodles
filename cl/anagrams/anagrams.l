(defun anagrams-internal (bag dict level)
  (when (zerop level)
    (format t "Pruning dictionary (~A bags); patience ..." (length dict))
    (finish-output))

  ;; This isn't necessary for correctness, but it makes a HUGE
  ;; difference in speed.
  (setf dict (let ((a 0))
               (mapcan #'(lambda (dict-entry)
                           (incf a)
                           (and ;(zerop (rem a 20))
                                (bag-reduce bag (car dict-entry))
                                (list dict-entry)))
                       dict)))


  (when (zerop level)
    (format t " done (reduced to ~A bags)!  Let's get started ...~%" (length dict))
    (finish-output))
  
  ;; It also might be fun to sort the dictionary by word size
  ;; (obviously we'd only need to do this once).  This would ensure
  ;; that the first anagrams we see have big words in them, and are
  ;; thus more interesting.
  
  (let ((rv '()))
    (maplist
     #'(lambda (subdict)
         (let* ((this-entry (car subdict))
                (this-bag (car this-entry))
                (these-words (cdr this-entry)))
           (when (< level 1) (format t "." this-entry)(finish-output))
           (let* ((dict-entry-bag this-bag)
                  (smaller-bag (bag-reduce bag dict-entry-bag)))

             (when smaller-bag
             
               (if (bag-emptyp smaller-bag)
                   (progn
                     (setq rv (nconc rv (mapcar #'list these-words)))                   
                     )
                 (let ((more-anagrams (anagrams-internal smaller-bag subdict (+ 1 level))))
                 
                   (when more-anagrams
                     (let ((with-new-word 
                            (mapcan #'(lambda (word-to-prepend)
                                        (mapcar #'(lambda (phrase) 
                                                    (cons word-to-prepend phrase))
                                                more-anagrams))
                                    these-words)))
                       (setq rv (nconc rv with-new-word))
                       (when (zerop level)
                         (format t "~%~A~&" with-new-word))))))))))
     dict)
    rv))

(defun anagrams (bag)
  (anagrams-internal bag *dict* 0))

(let ((dict (make-dictionary (list "hey" "you"))))
  (let ((a (anagrams-internal (bag "heyyou") dict 0)))
    (format t "Hey you => ~S~%" a)
    (assert (= 1 (length a)))))

(let ((dict (make-dictionary (list))))
    (assert (null (anagrams-internal (bag "I don't expect any!") dict 0))))

(let ((dict (make-dictionary (list "hey" "you"))))
  (assert (null (anagrams-internal (bag "") dict 0))))

(let* ((dict (make-dictionary (list "yeh" "you" "ouy")))
       (a (anagrams-internal (bag "youhey") dict 0)))
  (assert (= 2 (length a)))
  (assert (= 2 (length (car a)))))