(defvar *dict*)
(defun anagram-type (thing)
  (and (listp thing)
       (every #'(lambda (elt)
                  (and (listp elt)
                       (every #'stringp elt)))
              thing)))

(defvar *verbose* nil)

(defun combine (these-words more-anagrams)
  (mapcan #'(lambda (word-to-prepend)
              (mapcar #'(lambda (phrase) 
                          (cons word-to-prepend phrase))
                      more-anagrams))
          these-words))

(defmacro maybe-dump (ans)
  `(if (zerop depth)
       (dolist (a ,ans) (format t "~a~%" a))))

(defun anagrams-internal (bag exclusions depth)
  
  (let ((rv ()))
    (save-exclusions exclusions
     ;; Now find anagrams containing smaller words.
     (maphash
      #'(lambda (this-bag these-words)
          (unless (excluded? this-bag exclusions)
            (let ((smaller-bag (subtract-bags bag this-bag)))
              (when smaller-bag
                (if (bag-emptyp smaller-bag)
                    (progn
                      (add-exclusion! exclusions this-bag)
                      (let ((combined (mapcar #'list these-words)))
                        (maybe-dump combined)
                        (setf rv (append rv combined))))
                  (let ((more-anagrams (anagrams-internal smaller-bag exclusions (+ 1 depth))))
                    (check-type more-anagrams (satisfies anagram-type))
                    (when more-anagrams
                                        ;(format t "Prepending ~a to ~a~%" these-words more-anagrams)
                      (let ((combined (combine these-words more-anagrams)))
                        (maybe-dump combined)
                        (setf rv (append rv combined))
                        ;;(check-type rv (satisfies anagram-type))
                        (add-exclusion! exclusions this-bag)
                        ))))))))
      *dict*))

    ;;(check-type rv (satisfies anagram-type))

    rv))

(defun anagrams (string)
  (declare (type simple-base-string string))
  (let ((b  (bag string)))
    (init b)
    (let ((result (anagrams-internal b nil 0)))
      (prog1 result
            (format t ";; ~a anagrams of ~a~%" (length result)
              string)))))
