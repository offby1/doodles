(module frame
  mzscheme
  (require (lib "mred.ss" "mred"))
  (require (lib "class.ss"))
  
  (define frame (instantiate frame% ("All About Cats")
                  (width 200)
                  (height 200)))
  
  (send frame show #t) 
  (define menu-bar (instantiate menu-bar% () (parent frame)))
  (send menu-bar enable #t)
  (let ((cats-menu (instantiate menu% () (label "&Cats") (parent menu-bar))))

    (instantiate menu-item% ()
                 (label "&Really")
                 (parent (instantiate menu% () 
                                      (label "Rename things" )
                                      (parent menu-bar)))
                 (callback (lambda (item event) 
                             (for-each
                              (lambda (mi) 
                                (send mi set-label 
                                      (string-append 
                                       "Renamed " 
                                       (send mi get-label))))
                              
                              (send cats-menu get-items)))))
  
    (for-each
     (lambda (cat-name description shortcut-char)
       (send (instantiate menu-item% () 
                          (label cat-name)
                          (parent cats-menu)
                          (callback 
                           (lambda (item event) 
                             (message-box (format "Info about ~A" cat-name)
                                          description frame ))))
             set-shortcut shortcut-char))
   
     (list "Rusty" "Snowball")
     (list "Rusty is a red cat.  He's about three years old."
           "Snowball is white.  He is very cute.")
     (list #\r #\s)))
  
  (instantiate menu-item% () 
    (label "&About")
    (parent (instantiate menu% () (label "&Help") (parent menu-bar)))
    (callback (lambda (item event)
                (message-box "About Cats"
                             "This is pretty slick, if I do say so myself." frame)))))

(require frame)
