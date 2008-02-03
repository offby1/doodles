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

(= server* "192.168.0.126")
(def >err (msg . args)
  (w/stdout (stderr)
    (apply warn msg args)))

(def out args
  (apply prn args (list "\r"))
  (w/stdout (stderr)
    (apply prn "=>" args)))

((afn (nick)
      (let (ip op)
        (client server* 6667)
        (w/stdin ip
          (w/stdout op
            (out "NICK " nick)
            (prf "USER ~a unknown-host ~a :~a, version ~a"
                 (or (env "USER") "unknown")
                 server*
                 "arcbot"
                 "0")
            (out)

            (whilet l (readline)
              (let l (parse l)
                (case (car l)
                  NOTICE (>err  "ooh, a notice:" (cdr l))
                  PING   (out "PONG :" (cadr l))
                  (case (cadr l)
                    |433| (do (>err "Oh hell, gotta whop the nick.") (self (+ nick "_")))
                    (>err  "dunno wot to do" l))))
              )))))
 "arcbot")

; ":Chalain!n=chalain@216-74-233-198.res.logixcom.net QUIT :\"Lost terminal\"")
; -> (:Chalain!n=chalain@216-74-233-198.res.logixcom.net QUIT "\"Lost terminal\"")
;; (two symbols and a string)
(def parse (s)
  ;; Generally, a colon means "From here to the end of the line is a
  ;; single string".  Exceptions:
  ;; * the first character is sometimes a colon.  We deal with that below.
  ;; * sometimes a colon introduces a number, don't ask me why.  But
  ;; that only happens in lines that we ignore anyway :)

  (let toks (fn (s) (map sym (tokens s)))
    (aif (findsubseq ":" s 1) 
         (join (toks (subseq s 0 it))
               (list (subseq s (+ it 1))))
         (toks s))))
