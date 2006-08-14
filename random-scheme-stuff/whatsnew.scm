;; This file lists some directories which contain interesting
;; information.  It sorts them by date, most recent stuff first, and
;; only displays the five most recent items. That way I can see what's
;; new.

(require 'multiply)

(let (
      ;; Where the output will go.
      (tmpfile (string-append (getenv "tmp") "\\whatsnew"))

      ;; A command we call a lot.
      (dircmd "dir /n /o-d"))

  ;; Delete the output file.
  (system (string-append "del "
			 tmpfile
			 " > nul 2>&1"))

  (for-each

   ;; Given the name of a directory, return a command that appends the
   ;; newest entries in that directory to our output file.
   (lambda (string)
     (system (string-append
	      dircmd " " string " | head -10 >> " tmpfile)))

   ;; GLOM lets me generate all combinations of some strings.
   (let ((glom (lambda list-of-string-lists
                 (map (lambda (l)
                        (apply string-append l)) (apply multiply list-of-string-lists)))))
     (append
      (glom '("\\\\products2\\") '("beta\\" "rel") '("apps" "lang" "sys"))
      (glom '("\\\\") '("playtime" "bsdinfo") '("\\public"))
      (glom '("\\\\toolsvr\\") '("tools" "info"))
    
      '("\\\\products2\\other"
        "\\\\itg1\\release"
        "\\\\mmproducts2\\beta"
        "\\\\drg\\pub_tool"
        "\\\\infosrv2\\technet"
        "\\\\infosrv1\\msdn"
        "\\\\hank\\bhdist\\current"))))
     

  (system (string-append "list "
                         tmpfile)))
(quit)
