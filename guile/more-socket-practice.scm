(define (read-from-socket hostname port-number)
  (let ((s (socket AF_INET SOCK_STREAM 0)))

    (connect s AF_INET
             (car (hostent:addr-list (gethost hostname)))
             port-number)

    (let ((return (let loop ((so-far '()))
                    (if (char-ready? s)
                        (loop (cons (read-char s) so-far))
                      so-far))))

      (close-port s)
      return)))
