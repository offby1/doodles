(progn
  (load "sentences.l")
  (use-package :sb-profile)
  (sb-profile:profile survey-string
                      random-inclusively-between
                      update-template
                      spell-out-character 
                      template->string yow)
  (yow 2000) 
  (sb-profile:report)
  (quit))
