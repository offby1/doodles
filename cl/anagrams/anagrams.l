(defun anagrams-internal (bag length)
  
  (let ((rv ;; first simply look up all the words that are this size.
         (mapcar #'list (gethash bag (aref *dicts* length)))))
    (when (not (null rv))
      (format t "Found some easy ones: ~a~%" rv) )
    ;; Now find anagrams containing smaller words.
    (dotimes (this-length (- length 1) rv)
      (when (not (zerop this-length))
        (maphash
         #'(lambda (this-bag these-words)
             (unless (member (car these-words) rv :test #'(lambda (w seq) (member w seq :test #'equalp)))
               (multiple-value-bind (smaller-bag smaller-length)
                   (subtract-bags bag this-bag)
                 (when smaller-bag
                   (let ((more-anagrams (anagrams-internal smaller-bag smaller-length)))
                     (when more-anagrams
                       (format t "Prepending ~a to ~a~%"
                               these-words more-anagrams)
                       (let ((with-new-word 
                              (mapcan #'(lambda (word-to-prepend)
                                          (mapcar #'(lambda (phrase) 
                                                      (cons word-to-prepend phrase))
                                                  more-anagrams))
                                      these-words)))
                         (setq rv (nconc rv with-new-word)))))))))
         (aref *dicts* this-length))))
    rv))

(defun anagrams (string)
  (declare (type simple-base-string string))
  (multiple-value-call #'anagrams-internal (bag string)))
