(module frame
    mzscheme
  (require (lib "mred.ss" "mred"))
  ;;(require (lib "framework.ss" "framework")) 
  (require (lib "class.ss"))
  (define frame (instantiate frame% ("All About Cats")
                             (width 200)
                             (height 200)))

  (send frame show #t) 
  (define menu-bar (instantiate menu-bar% () (parent frame)))
  (send menu-bar enable #t)
  (define cats-menu   (instantiate menu% () (label "&Cats") (parent menu-bar)))
  (define help-menu  (instantiate menu% () (label "&Help") (parent menu-bar)))
  (instantiate menu-item% () 
               (label "&About")
               (parent help-menu)
               (callback (lambda (item event)
                           (message-box "About Cats"
                                        "This is pretty slick, if I do say so myself." frame))
                       
                         ))

  (define rename-menu (instantiate menu% () (label "&Rename things" )
                                   (parent menu-bar)
                                   ;;(demand-callback (lambda (whatever) (message-box "Aha!" "So this is what a demand-callback is." #f)))
                                   ))
  (for-each
   (lambda (cat-name description shortcut-char)
     (let ((menu-item
            (instantiate menu-item% () 
                         (label cat-name)
                         (parent cats-menu)
                         (callback (lambda (item event) 
                                     (message-box (format "Info about ~A" cat-name)  description frame ))))))
     
       (send menu-item set-shortcut shortcut-char)))
 
   (list "Rusty" "Snowball")
   (list "Rusty is a red cat.  He's about three years old."
         "Snowball is white.  He is very cute.")
   (list #\r #\s))
  )