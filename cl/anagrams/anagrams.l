(defun anagrams-internal (bag length)

  
  ;; It also might be fun to sort the dictionary by word size
  ;; (obviously we'd only need to do this once).  This would ensure
  ;; that the first anagrams we see have big words in them, and are
  ;; thus more interesting.
  
  (let ((rv ;; first simply look up all the words that are this size.
         (mapcar #'list (gethash bag (aref *dicts* length)))))

    ;; Now find anagrams containing smaller words.
    (dotimes (this-length (- length 1) rv)
      (when (not (zerop this-length))
        (setf rv (append rv
                         (maphash
                          #'(lambda (this-bag these-words)
                              (multiple-value-bind (smaller-bag smaller-length)
                                  (subtract-bags bag this-bag)
                                (when smaller-bag
                                  (if (zerop smaller-length)
                                      (progn
                                        (setf rv (nconc rv (mapcar #'list these-words)))
                                        (format t "Woo hoo! ~s~%" rv))
                                    (let ((more-anagrams (anagrams-internal smaller-bag smaller-length)))
                                      (when more-anagrams
                                        (let ((with-new-word 
                                               (mapcan #'(lambda (word-to-prepend)
                                                           (mapcar #'(lambda (phrase) 
                                                                       (cons word-to-prepend phrase))
                                                                   more-anagrams))
                                                       these-words)))
                                          (setq rv (nconc rv with-new-word)))))))))
                          (aref *dicts* this-length))))))
    rv))
