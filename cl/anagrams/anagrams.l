

#+cmu (setq *gc-verbose* nil)

(defconstant *characters*
  (+ 1 (- (char-code #\z) (char-code #\a)))
  "number of characters that we distinguish.")

(defun survey (string)
  (bag:string->bag string))

(defun survey-subtract (big small)
  (let ((result (bag:subtract big small)))
    (if (not (bag:empty result))
        result
      nil)))

;;(trace survey-subtract)

(defun bag-emptyp (bag)
  (bag:empty bag))

(defun find-words (bag dictionary)
  (mapcan #'(lambda (one-word)
	      (and
	       (survey-subtract bag (survey one-word))
	       (list one-word)))
	  dictionary))

(defun prepend-word (word lol)
  (assert (stringp word))
  (assert (listp lol))
  (assert (listp (car lol)))
  (map 'list #'(lambda (wordlist)
		 (cons word wordlist))
       lol))

(defun anagrams (string dictionary)

  (defun anagram-helper (bag dictionary)

    (if (bag-emptyp bag)
	(return-from anagram-helper nil))

    (let ((dictionary (find-words bag dictionary)))
    
      (if (null dictionary)
	  (return-from anagram-helper nil))
     
      (apply #'append
	     (map 'list #'(lambda (one-word)
			    (let ((smaller-bag (survey-subtract bag (survey one-word))))
			      (assert smaller-bag)
			      (if (bag-emptyp smaller-bag)
				  (list (list one-word))
				(let ((dictionary dictionary))

				  (loop

				   ;; ignore dictionary items that
				   ;; precede one-word, since they'll
				   ;; have already been processed.
				   (if (or
					(null dictionary)
					(string<= one-word
						 (car dictionary)))
				       (return))
				   (setq dictionary (cdr dictionary)))
  
				  (let ((from-smaller-bag (anagram-helper smaller-bag dictionary)))
				    (and from-smaller-bag
					 (prepend-word one-word
						       from-smaller-bag)))))))
		  dictionary))))

  (anagram-helper (survey string) dictionary))
