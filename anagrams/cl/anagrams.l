(defvar *dict*)
(defun anagram-type (thing)
  (and (listp thing)
       (every #'(lambda (elt)
                  (and (listp elt)
                       (every #'stringp elt)))
              thing)))

(defun anagrams-internal (bag exclusions)
  
  (let ((rv ;; first simply look up all the words that are this size.
         (mapcar #'list (gethash bag *dict*))))

    ;;(check-type rv (satisfies anagram-type))

    (when (not (null rv))
      (format t "Found some easy ones: ~a~%" rv)
      (setf (gethash bag exclusions) t))
    ;; Now find anagrams containing smaller words.
    (maphash
     #'(lambda (this-bag these-words)
         (unless (gethash this-bag exclusions)
           (let ((smaller-bag (subtract-bags bag this-bag)))
             (when smaller-bag
               (let ((more-anagrams (anagrams-internal smaller-bag exclusions)))
                 (check-type more-anagrams (satisfies anagram-type))
                 (when more-anagrams
                   (format t "Prepending ~a to ~a~%"
                           these-words more-anagrams)
                   (let ((with-new-word 
                          (mapcan #'(lambda (word-to-prepend)
                                      (mapcar #'(lambda (phrase) 
                                                  (cons word-to-prepend phrase))
                                              more-anagrams))
                                  these-words)))
                     (setf rv (nconc rv with-new-word))
                     ;;(check-type rv (satisfies anagram-type))
                     (setf (gethash this-bag exclusions) t))))))))
     *dict*)

    ;;(check-type rv (satisfies anagram-type))

    rv))

(defun anagrams (string)
  (declare (type simple-base-string string))
  (let ((b  (bag string)))
    (init b)
    (anagrams-internal b (make-hash-table :test #'equalp))))
