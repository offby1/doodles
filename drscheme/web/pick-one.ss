(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
         (lib "url.ss" "net")
         (list "list.ss"))

(unit/sig () (import servlet^)
  (report-errors-to-browser (lambda (response)
                              (send/back
                               `(html (head (title "oops"))
                                  (body (p ,(format "You blew it: ~s" response))
                                        )))))
  (define (radio-pick-one prompt choices)
    (let ((tmp  (assoc 'answer
                       (request-bindings
                        (send/suspend
                         (lambda (k-url)
                           `(html (head (title ,prompt))
                                  (body (p ,prompt)
                                        (form ((method "post") (action ,k-url))
                                              ,@(map (lambda (choice)
                                                       `(p
                                                         ,(symbol->string choice)
                                                         (input ((type "radio")
                                                                 (name "answer")
                                                                 (value ,(symbol->string choice))))))
                                                     choices)
                                              (input ((type "submit")
                                                      (value "Hokay"))))))))))))
      (if tmp
          (cdr tmp)
        'you-bozo-you-didnt-pick-anything)))

  (define (links-pick-one prompt choices)
    (let ((tmp  (assoc 'answer
                       (request-bindings
                        (send/suspend
                         (lambda (k-url)
                           (define (build-url name value)
                             (let ((url (string->url k-url)))
                               (set-url-query! url 
                                        ; BUGBUG -- we need to URI-escape both name and value.
                                               (format "~a=~a" name value)
                                               )
                               (url->string url)))
                           `(html (head (title ,prompt))
                                  (body (p ,prompt)
                                        ,@(map (lambda (choice)
                                                 `(p (a ((href ,(build-url "answer" choice))) ,(symbol->string choice))))
                                               choices)))))))))
      (if tmp
          (cdr tmp)
        'you-bozo-you-didnt-pick-anything)))

  (let loop ((choices '(dog cat ferret skunk)))
    (if (null? choices)
        (send/finish
         `(html
           (head "All done!")
           (body (p "You've picked everything."))))    
      
      (let ((c (links-pick-one "Pick the cutest small furry mammal" choices)))
        (send/back
         `(html
           (head "Here's what you picked")
           (body (p
                  ,(format "~s" c)))))
        (loop (remove c choices)))))
