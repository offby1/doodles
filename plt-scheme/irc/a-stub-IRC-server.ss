#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module a-stub-IRC-server mzscheme
(require "globals.ss")
(define *server-name* "stub-IRC-server.")
;; An IRC server that doesn't do very much at all.  It's for testing
;; the client.
(define (stub-irc-server)
  (let-values (((readme writeme)
                (make-pipe)))
    (thread
     (lambda ()
       (define (PRIVMSG str private?)
         (fprintf
          writeme
          (format
           ":Jan-!n=Hfuy@82.152.177.104 PRIVMSG ~a :~a~a~%"
           (if private? *my-nick* "#some-channel")
           str
           #\return))
         (flush-output writeme))
       (define (private-message str)
         (PRIVMSG str #t))
       (define (channel-message str)
         (PRIVMSG str #f))
       (define (reply number blather)
         (fprintf
          writeme
          ":~a ~a ~a :~a~a~%"
          *server-name*
          number
          *my-nick*
          blather
          #\return))

       (reply "001" (format "Welcome to the magical internal stub testing IRC server, ~a" *my-nick*))

       (reply
        "353"
        (format
         "~a = #some-channel :~a Odin- feklee cmo-0 Meyvn sanelson hrehf blandest capitali2ea sentor2 RetroJ``` ecraven Arixx shash_ defcons iblechbot qrck mejja dfa mboes jewel_ IceD^ chris2 l_a_m pft ptx pocket12 loquace Yuuhi meandtheshell jrockway xinming emss sybarite1 Johbe quicksilver nunag hkBst palo Athas Tristan polk__ jah ramenboy sabetts isomer tizoc octoberdan b0ef AnMaster_ chessguy oxymor00n hellwolf_ Yawgmoth7 nym daniel_ jbms hedos syamajala merriam__ "
         *my-nick*
         *my-nick*))
       (private-message "what up")
       (private-message "\u0001VERSION\u0001")
       (channel-message "\u0001ACTION glances around nervously.\u0001")
       (fprintf
        writeme
        (format
         ":Jan-!n=Hfuy@82.152.177.104 NOTICE #some-channel :\u0001ACTION notices someone glancing.\u0001~a~%"
         #\return))
       (channel-message "Hey, everybody!  Let's put on a show.")
       (channel-message (format "~a: You are dumb." *my-nick*))
       (fprintf
        writeme
        "PING :~a~a~%"
        *server-name*
        #\return)
       (channel-message (format "~a: quote"  *my-nick*))
       (channel-message (format "~a: census" *my-nick*))
       (private-message "OK, that's all.")
       (close-output-port writeme)
       ))
    (values readme (current-output-port))))
(provide stub-irc-server)
)