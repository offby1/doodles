(define known-hosts
  (begin
    (sethostent)
    (let loop ((hosts '())
               (a-host (gethostent)))
      (if (not a-host)
          (begin
            (endhostent)
            (reverse hosts))
        (loop (cons a-host hosts)
              (gethostent))))))

(define (prettify host)
  (list
   (hostent:name host)
   (hostent:aliases host)
   (map inet-ntoa (hostent:addr-list host))))
