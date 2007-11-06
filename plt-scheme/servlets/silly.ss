(module silly mzscheme
(require (lib "servlet.ss" "web-server"))
(provide interface-version timeout start)

;; last time I checked, I lived at
;; /usr/local/stow/plt.371/lib/plt/collects/web-server/default-web-root/servlets/silly.ss

(define interface-version 'v1)
(define timeout +inf.0)

(define (get-username/pw)
  (let ((bindings
         (request-bindings
          (send/suspend
           (lambda (k-url)
             `(html
               (body (h1 "Time for some user name and password action!")
                     (form ((action ,k-url))
                           (input ((type "text") (name "username") (id "username")))
                           (input ((type "password") (name "password") (id "password")))
                           (input ((type "submit")))))))))))
    (values
     (extract-binding/single 'username bindings)
     (extract-binding/single 'password bindings))))

(define (start initial-request)
  (send/suspend
   (lambda (k-url)
     `(html
       (body
        (p "Click this pointless button to continue.")
        (form ((action ,k-url))
              (input ((type "submit"))))))))
  (send/finish
   (let-values (((u p)
                 (get-username/pw)))
     `(html
       (body
        (p ,(format "your user name is ~a ...." u))
        (p ,(format "your password is ~a." p)))))))
)