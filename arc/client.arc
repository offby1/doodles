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
      (prf "USER ~a unknown-host ~a :~a, version ~a"
           (or (env "USER") "unknown")
           server*
           "arcbot"
           "0")

      (whilet l (readline)
        (w/stdout (stderr)
        (warn "Server say:" l)))
      )))