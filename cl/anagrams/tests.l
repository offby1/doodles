(si::use-fast-links nil)
(defun unit ()
  (let ((attempted 0)
	(passed 0)
	(failed 0))
    (defun expect (func args expected-output)
      (incf attempted)
      (let ((actual-output (apply func args)))
	(if (not (equalp expected-output
			 actual-output))
	    (progn
	      (write (format nil "FAIL: args ~S yielded output ~S, but expected ~S" 
			     args actual-output expected-output))
	      (terpri)
	      (incf failed))

	  (progn
	    (princ ".")
	    (incf passed)))))
    
    (expect #'survey '("zzz") #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 ))
    (expect #'survey '("abc") #(1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ))
    (expect #'survey '(""   ) #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ))

    (expect #'prepend-word '("bob" nil) nil)
    (expect #'prepend-word '("bob" (("sam"))) '(("bob" "sam")))
    (expect #'prepend-word '("bob" (("sam" "hill")
				    ("fred" "astaire")))
	    '(("bob" "sam" "hill")
	      ("bob" "fred" "astaire")))

    (let ((abc (survey "abc"))
	  (aa  (survey "aa"))
	  (a   (survey "a"))
	  (bc  (survey "bc"))
	  (empty (survey "")))
      (expect #'survey-subtract (list empty empty) empty)
      (expect #'survey-subtract (list abc aa)      nil)
      (expect #'survey-subtract (list aa abc)      nil)
      (expect #'survey-subtract (list abc abc)     empty)
      (expect #'survey-subtract (list aa aa)       empty)
      (expect #'survey-subtract (list aa a)        a)
      (expect #'survey-subtract (list abc a)       bc)
      )
  
    (let ((dict  '("a" "be" "cat" "do" "go" "me" "now")))

      (defun test (input expected-output)
	(expect #'anagrams
		(list input dict)
	      
		expected-output))
    
      (expect #'find-words (list (survey "") dict) nil)
      (expect #'find-words (list (survey "abe") dict) '("a" "be"))
      (expect #'find-words (list (survey "bea") dict) '("a" "be"))
      (expect #'find-words (list (survey "z") dict) nil)

      (test "abe" '(("a" "be")))
    
      (test "be cat cat" '(("be" "cat" "cat")))
      (test "xyzzy" nil)
      (test "cat" '(("cat")))
      (test "catx" nil)
      (test "xcat" nil))
    (terpri)
    (princ (format nil "Attempted: ~A" attempted)) (terpri)
    (princ (format nil "Passed   : ~A" passed)) (terpri)
    (princ (format nil "Failed	 : ~A" failed)) (terpri)
    (values)))
