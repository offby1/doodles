;; Note that unless we use a stripped-down dictionary, CMU Common Lisp
;; release x86-linux 3.1.2 18d+ 24 September 2002 build 4293 dies with
;; what appears to be an assertion failure in the garbage collector.
;; However, CMU Common Lisp 18e works fine.

;; sbcl SBCL 0.7.2 does too!

;; clisp 2.27, however, completes with no trouble.  Pity it's slower
;; than the others ...

(declaim (optimize speed))
                          
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

#+clisp (ext:space (format t "~A~%" (anagrams (bag "ernesthemingway"))))

