(defvar *dict*
  (with-open-file
   (dict "/usr/share/dict/words")
   (let ((d '()))
     (loop
      (let ((datum (read-line dict nil nil)))
        (if (null datum)
            (return d))
        (setq d (cons datum d)))))))

#+cmu (setq *gc-verbose* nil)

(defconstant *characters*
  (+ 1 (- (char-code #\z) (char-code #\a)))
  "number of characters that we distinguish.")

(defun survey (string)
  "returns a vector.  Each slot represents a lower-case character; the
value in the slot is the number of occurences of *either* the upper-
or lower-case version of that character in STRING."
  #+cmu (declare (type (simple-array base-char *) string))
  #+cmu (declare (optimize (speed 3)))
  ;;(assert (stringp string))

  (let ((result (make-array *characters* :element-type 'fixnum :initial-element 0)))
    (let ((chars-to-examine (length string)))
      #+cmu (declare (type fixnum chars-to-examine))
      (loop
       (if (zerop chars-to-examine)
	   (return result)
	 (let ((ch (aref string (1- chars-to-examine))))
	   (if (alpha-char-p ch)
	       (incf (aref result
                           (- (char-code (char-downcase ch))
                              (char-code #\a))
                           )))))
       (decf chars-to-examine)))
    result))

(defun survey-subtract (big small)
  (let ((result (survey ""))
	(slots-examined 0))
    (loop
     (if (= slots-examined *characters*)
	 (return result))
     (let ((diff (- (aref big slots-examined)
		    (aref small slots-examined))))
       (if (minusp diff)
	   (return nil))
       (setf (aref result slots-examined)
	     diff))
     (incf slots-examined))))

(defun bag-emptyp (bag)
  (and (not (null bag))
       (zerop (apply #'+ (map 'list #'identity bag)))))

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
