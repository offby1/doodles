(let ((*error-output*  *standard-output*))
  (defvar *dict* (make-dictionary "/usr/share/dict/words"))
  (format t "dictionary contains ~A words ...~%" (length *dict*))

  ;; Because it's so damned slow, throw out most of the dictionary ...
  ;; so it will run faster.  Otherwise it's too slow to even profile.

  (when t

    (setq *dict* (let ((words-to-examine (length *dict*))
                       (reduced-dictionary '()))
                   (loop
                    (when (zerop words-to-examine)
                      (return reduced-dictionary))
                    (let ((word (car *dict*)))
                      (when (zerop (rem words-to-examine 20))
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

  )

#+cmu (progn 
        (profile:profile-all) 
        ;;(trace anagrams-internal)
        (anagrams (bag "ernest hemingway"))
        (profile:report-time)

        )
#+sbcl (progn 
         ;; grr ... sb-profile:profile is a macro, so I can't use mapcar
         (SB-PROFILE:PROFILE anagrams)
         (sb-profile:profile anagrams-internal)
         (sb-profile:profile bag)
         (sb-profile:profile bag-add)
         (sb-profile:profile bag-emptyp)
         (sb-profile:profile bag-reduce)
         (sb-profile:profile bag-to-string)
         (sb-profile:profile hash-copy)
         (anagrams (bag "ernest hemingway"))
         (SB-PROFILE:REPORT))

#+clisp (let ((result))
          (ext:space  (anagrams (bag "ernest hemingway"))))

