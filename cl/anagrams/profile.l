(let ((*error-output*  *standard-output*))
  (defvar *dict* (make-dictionary "/usr/share/dict/words"))
  (format t "dictionary contains ~A words ...~%" (length *dict*))

  (format t "dictionary (~A -> ~A) contains ~A words ...~%" 
          (first *dict*)
          (last *dict*)
          (length *dict*)))

#+cmu (progn 
        (profile:profile-all) 
        ;;(trace anagrams-internal)
        (format t "~%~A~&" (anagrams (bag "ernesthemingway")))
        (profile:report-time)

        )
#+sbcl (progn 
         ;; grr ... sb-profile:profile is a macro, so I can't use mapcar
         (SB-PROFILE:PROFILE anagrams)
         (sb-profile:profile anagrams-internal)
         (sb-profile:profile bag)
         (sb-profile:profile bag-emptyp)
         (sb-profile:profile bag-reduce)
         (sb-profile:profile bag-to-string)
         (sb-profile:profile hash-copy)
         (format t "~%~A~&" (anagrams (bag "ernesthemingway")))
         (SB-PROFILE:REPORT))

#+clisp (let ((result))
          (ext:space  (anagrams (bag "ernesthemingway"))))

