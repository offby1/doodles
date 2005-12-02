(in-package :anagrams)

(defvar *dict*)
(defvar *verbose* nil)

(defun combine (these-words more-anagrams)
  (mapcan #'(lambda (word-to-prepend)
              (mapcar #'(lambda (phrase) 
                          (cons word-to-prepend phrase))
                      more-anagrams))
          these-words))

(defmacro maybe-dump (ans)
  `(if (zerop depth)
       (dolist (a ,ans) (format *error-output* "~a~%" a))))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun anagrams-internal (bag dict depth)
  
  (let ((rv ()))
    (while (not (null dict))
      (let* ((entry (car dict))
             (this-bag (car entry))
             (these-words (cdr entry))
             (smaller-bag (subtract-bags bag this-bag)))
        (when smaller-bag
          (if (bag-emptyp smaller-bag)
              (let ((combined (mapcar #'list these-words)))
                ;(maybe-dump combined)
                (setf rv (append rv combined)))
              (let ((more-anagrams (anagrams-internal smaller-bag dict (+ 1 depth))))

                (when more-anagrams
                  (let ((combined (combine these-words more-anagrams)))
                    ;(maybe-dump combined)
                    (setf rv (append rv combined))))))))
      (setf dict (cdr dict)))

    rv))

(defun anagrams (string)
  (let ((b  (bag string)))
    (init b)
    (let ((result (anagrams-internal b *dict* 0)))
      (prog1 result
        (format *error-output* ";; ~a anagrams of ~a~%" (length result)
                string)))))
