(defvar *dict* (make-dictionary "/usr/share/dict/words"))
(format t "dictionary contains ~A words ...~%" (length *dict*))

;; Because it's so damned slow, throw out most of the dictionary ...
;; so it will run faster.  Otherwise it's too slow to even profile.

(when nil

  (setq *dict* (let ((words-to-examine (length *dict*))
                     (reduced-dictionary '()))
                 (loop
                  (when (zerop words-to-examine)
                    (return reduced-dictionary))
                  (let ((word (car *dict*)))
                    (when (zerop (rem words-to-examine 10))
                      (setf reduced-dictionary (cons word reduced-dictionary))))
                  (setf *dict* (cdr *dict*))
                  (decf words-to-examine))))

  (setq *dict* (sort (nconc 
                      *dict* 
                      (list "anywhere" "me" "sting" )) #'string-lessp)))

;; If we don't throw out most of the dictionary, as above,
;; CMU Common Lisp release x86-linux 3.1.2 18d+ 24 September 2002 build 4293
;; dies with what appears to be an assertion failure in the garbage collector.

(format t "dictionary (~A -> ~A) contains ~A words ...~%" 
        (first *dict*)
        (last *dict*)
        (length *dict*))

#+cmu (progn 
        (profile:profile-all) 
        ;;(trace anagrams-internal)
        (format t "~&~A~%" (anagrams (bag "ernest hemingway")))
        (profile:report-time)
        )

#+clisp (let ((result))
          (ext:space (setq result (anagrams (bag "ernest hemingway"))))
          (format t "~&~A~%" result))
