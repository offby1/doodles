;; I guess this is really scheme ...
($ 
 (xdef 'client 
       (lambda (hostname port)
               (call-with-values 
                (lambda () (tcp-connect hostname port)) 
                (lambda (ip op)
                        (list ip op))))) )

;; be nice to make this settable, so we could do (= (env "FOO") "bar")
($ (xdef 'env getenv))

(= server* "localhost")
(let (ip op)
  (client server* 6667)
  (w/stdin ip
    (w/stdout op
      (prn "NICK arcbot")
      (prf "USER ~a unknown-host ~a :~a, version ~a\n"
           (or (env "USER") "unknown")
           server*
           "arcbot"
           "0")

      ;; TODO:
      ;; * install a proper thing-what-deals-with-lines-from-the-server
      ;; * write a PONG thread
      (whilet l (readline)
        (w/stdout (stderr)
        (warn "Server say:" (parse l))))
      )))

(= s ":Chalain!n=chalain@216-74-233-198.res.logixcom.net QUIT :\"Lost terminal\"")

(def parse (s)
  ;; Generally, a colon means "From here to the end of the line is a
  ;; single string".  Exceptions:
  ;; * the first character is sometimes a colon.  We deal with that below.
  ;; * sometimes a colon introduces a number, don't ask me why.  But
  ;; that only happens in lines that we ignore anyway :)
  (awhen (findsubseq ":" s 1) 
    (join (map [coerce _ 'sym] (tokens (subseq s 0 it)))
          (list (subseq s (+ it 1))))))
