;; I guess this is really scheme ...
($ 
 (xdef 'client 
       (lambda (hostname port)
               (call-with-values 
                (lambda () (tcp-connect hostname port)) 
                list))))

;; be nice to make this settable, so we could do (= (env "FOO") "bar")
($ (xdef 'env getenv))

(= server* "localhost")
(def >err (msg . args)
  (w/stdout (stderr)
    (apply prn msg args)))

(def out args
  (let args (join args (list "\r"))
    (apply prn args )
    (disp "=>" (stderr))
    (map [write _ (stderr)] args))
  nil)

(= chans* (table))
((afn (nick)
      (let (ip op)
        (client server* 6667)
        (w/stdin ip
          (w/stdout op
            (out "NICK " nick)
            (out
             "USER "          (or (env "USER") "unknown")
             " unknown-host " server*
             " :"             "arcbot"
             ", version "     "0")

            (whilet l (trim (readline) 'end)
              (>err "<=" l)
              (let l (parse l)
                (case (car l)
                  NOTICE (>err  "ooh, a notice:" (cdr l))
                  PING   (out "PONG :" (cadr l))
                  (case (cadr l)
                    |001|    (map [out "JOIN " _]  (list "#fart" "#poop"))
                    |433|    (do (>err "Oh hell, gotta whop the nick.")
                                 (self (+ nick "_")))
                    JOIN     (>err "user" (car l) "joined" (car:cdr:cdr l))
                    PRIVMSG  (withs ((speaker privmsg dest text) l
                                     toks (tokens text))
                                    (if (headmatch nick (car toks))
                                        (out "PRIVMSG " dest " :" speaker ", you like me!" )
                                        (out "PRIVMSG " dest " :yeah, whatever")))
                    (>err "?"))))
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
