#+gcl (si::use-fast-links nil)
(defun unit ()
  (let ((attempted 0)
	(passed 0)
	(failed 0))
    (defun expect (func args expected-output)
      (incf attempted)
      (let ((actual-output (apply func args)))
	(assert (equalp expected-output actual-output) nil 
                "FAIL: args ~S yielded output ~S, but expected ~S" 
                args actual-output expected-output)))
    
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
      (defun expect-diff (minuend subtrahend expected-content)
        (let ((actual-output (bag::subtract minuend subtrahend)))
          (assert (equalp expected-content (bag::as-alist actual-output)) nil 
                  "FAIL: subtracting ~S from ~S yielded output ~S, but expected ~S" 
                  subtrahend minuend actual-output expected-content))
        (expect #'survey-subtract (list empty empty) nil))
      (expect-diff empty empty nil)
      (expect-diff abc aa      nil)
      (expect-diff aa abc      nil)
      (expect-diff abc abc     nil)
      (expect-diff aa aa       nil)
      (expect-diff aa  a (bag::as-alist a))
      (expect-diff abc a (bag::as-alist bc))
      )))

