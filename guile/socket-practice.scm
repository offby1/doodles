#!/home/work/guile-core/bin/guile -s
!#

;; Practice making a server connection.
(let ((s (socket AF_INET SOCK_STREAM 0)))
  (define http-port 8000)               ; bug -- this should be looked
                                        ; up with getsrvbyname
  (bind s AF_INET INADDR_ANY http-port) ; gotta be root to do this if
                                        ; http-port < 1024
  (listen s 1)
  (let* ((new-connection (accept s))
         (p (car new-connection))
         (client-addr (cdr new-connection)))
    (display "Got a connection from `")
    (display client-addr)
    (display "'")
    (newline)
    ;; process all the data from this connection
    (for-each
     display
     (list
      #\newline
      "`"
      (let loop ((ch (read-char p))
                 (so-far ""))
        (cond
         ((eof-object? ch)
          so-far)
         ((not (char-ready? p))
          (string-append so-far (string ch)))
         (#t
          (loop (read-char p)
                (string-append so-far (string ch))))))
    
      "'"
      #\newline))
    
    ;; Now send some dummy HTML
    (display "<HTML>Hey you!\r\n</HTML>\r\n" p)
    (force-output p)
    (shutdown p 1)
    )
  (gc))

;; This practices making a client connection.
(let
    ;; create a socket
    ((s (socket AF_INET SOCK_STREAM 0)))

  ;; Just out of curiosity -- bind this socket, even though it's a
  ;; client.  I'm wondering if certain ports cause my Cisco 675 router
  ;; to break.
  ;;(bind s AF_INET (car (hostent:addr-list (gethostbyaddr "127.0.0.1"))) 1025)

  (connect s AF_INET
           (car (hostent:addr-list (gethost "ftp.blarg.net")))
           21                           ; ftp server
           )

  ;; read as much as possible from it, and display what we read
  (for-each
   display
   (list
    #\newline
    "`"
    (let loop ((line (read-line s 'concat))
               (so-far ""))
      (cond
       ((eof-object? line)
        so-far)
       ((not (char-ready? s))
        (string-append so-far line))
       (#t
        (loop (read-line s 'concat)
              (string-append so-far line)))))
    
    "'"
    #\newline))
  
  ;; close it and clean up

  (close-port s))