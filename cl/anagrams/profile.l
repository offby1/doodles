(defvar *dict* (make-dictionary "/usr/share/dict/words"))
(format t "dictionary contains ~A words ...~%" (length *dict*))
(setq *dict* (let ((words-to-examine (length *dict*))
                   (reduced-dictionary '()))
               (loop
                (when (zerop words-to-examine)
                  (return reduced-dictionary))
                (let ((word (car *dict*)))
                  (when (zerop (rem words-to-examine 50))
                    (setf reduced-dictionary (cons word reduced-dictionary))))
                (setf *dict* (cdr *dict*))
                (decf words-to-examine))))

(setq *dict* (sort (nconc 
                    ;;'()
                    *dict* 
                    (list "anywhere" "me" "sting" )) #'string-lessp))

(format t "dictionary (~A -> ~A) contains ~A words ...~%" 
        (first *dict*)
        (last *dict*)
        (length *dict*))

#+cmu (progn 
        (profile:profile-all) 
        ;;(trace anagrams-internal)
        (format t "~A~%" (anagrams (bag "ernest hemingway")))
        (profile:report-time)
        )

#+clisp (let ((result))
          (ext:space (setq result (anagrams (bag "ernest hemingway"))))
          (format t "~&~A~%" result))
