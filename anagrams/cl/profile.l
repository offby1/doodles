(declaim (optimize speed))

(mapcar #'(lambda (basename)
            (compile-file (concatenate 'string basename ".l"))
            (load (concatenate 'string basename
                               #+cmu  ".x86f"
                               #+sbcl ".fasl"
                               #+clisp ".fas"
                               )))
        `("exclusions"
          "numeric-bag"
          ;"bag"
          "dict"
          "anagrams"))

#+cmu (progn 
        (profile:profile anagrams-internal)
        (profile:profile anagrams)
        (profile:profile bag)
        (profile:profile subtract-bags)
        (profile:profile bag-emptyp)
        (profile:profile word-acceptablep)
        (profile:profile maybe-add-word)
        (profile:profile make-dictionary-from-file)
        (profile:profile make-dictionaries-from-list)
        (profile:profile init))

#+sbcl (progn 
         ;; grr ... sb-profile:profile is a macro, so I can't use mapcar
         (sb-profile:profile anagrams-internal)
         (sb-profile:profile anagrams)
         (sb-profile:profile bag)
         (sb-profile:profile subtract-bags)
         (sb-profile:profile bag-emptyp)
         (sb-profile:profile word-acceptablep)
         (sb-profile:profile maybe-add-word)
         (sb-profile:profile make-dictionary-from-file)
         (sb-profile:profile make-dictionaries-from-list)
         (sb-profile:profile init))

(let ((*print-level* nil)
      (*print-length* nil))
  #-clisp            (format t "Done!~%~A~&" (anagrams  "Ernest Hemi"))
  #+clisp (ext:times (format t "Done!~%~A~&" (anagrams  "Ernest Hemi")))
  )
#+sbcl (sb-profile:report)
#+cmu  (profile:report-time)
(quit)
