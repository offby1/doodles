;; To use me:

;; make a symlink to me from the web server's "servlets" directory,
;; e.g. /usr/local/src/langs/scheme/plt/collects/web-server/default-web-root/servlets/.
;; Start the server by running "web-server".
;; point your web browser at http://localhost/servlets/pick-one.ss

(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (only  (lib "url.ss" "net") string->url url->string set-url-query!))

(unit/sig () (import servlet^)
          (report-errors-to-browser (lambda (response)
                                      (send/back
                                       response)))
          (define (radio-pick-one prompt choices)
            (let* ((bindings (request-bindings
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
                                                            (value "Hokay"))))))))))
                   (tmp  (assoc 'answer bindings)))
              (if tmp
                  (cdr tmp)
                (format "Bindings: ~s~%" bindings))))

          (define (links-pick-one prompt choices)
            (let* ((bindings (request-bindings
                              (send/suspend
                               (lambda (k-url)
                                 (define (build-url name value)
                                   (let ((url (string->url k-url)))
                                     (set-url-query! url 
                                                     (list (cons (string->symbol name)
                                                                 (symbol->string value))))
                                     (url->string url)))
                                 `(html (head (title ,prompt))
                                        (body (p ,prompt)
                                              ,@(map (lambda (choice)
                                                       `(p (a ((href ,(build-url "answer" choice))) ,(symbol->string choice))))
                                                     choices)))))))
                   (tmp  (assoc 'answer
                                bindings)))
              (if tmp
                  (cdr tmp)
                (format "Bindings: ~s" bindings))))

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
                (loop (remove c choices))))))
