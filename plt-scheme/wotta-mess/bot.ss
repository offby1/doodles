#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; http://tools.ietf.org/html/rfc1459

(module bot mzscheme
(require
         (lib "trace.ss")
         (only (lib "pregexp.ss") pregexp-quote)
         (only (lib "1.ss" "srfi")
               filter
               first
               second
               take
               third
               )
         (only (lib "13.ss" "srfi")
               string-join
               string-tokenize
               )
         (only (lib "14.ss" "srfi")
               char-set
               char-set-complement
               char-set:whitespace)
         (only (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
               sxpath)
         (only (planet "zdate.ss" ("offby1" "offby1.plt")) zdate)
         "../web/quote-of-the-day.ss"
         "globals.ss"
         "parse-message.ss"
         "planet-emacs-task.ss"
         "quotes.ss"
         "task.ss"
         "vprintf.ss"
         )
(provide
 *tasks-by-channel*
 do-startup-stuff
 put
 respond)

(define (split str)
  (string-tokenize str (char-set-complement char-set:whitespace)))

(define (strip-leading-colon str)
  (if (char=? #\: (string-ref str 0))
        (substring str 1)
      str))

(print-hash-table #t)

;; http://tools.ietf.org/html/rfc1459#section-2.3.1
(define (parse-prefix str)
  (if (not str)
      '(#f #f #f)

    (let loop ((result (string-tokenize str (char-set-complement (char-set #\! #\@)))))
      (if (<= (length result ) 3)
          result
        (loop (cons #f result))))))

(define-struct utterance (when what action?) (make-inspector))

;; this will get set to a function that calls PUT, with the proper
;; output port.  It's useful from the REPL.
(define p* #f)

(define (put str op)
  (let ((str (substring str 0 (min 510 (string-length str)))))
    (vtprintf "=> to op ~s ~s~%" (object-name op) str)
    (display str op)
    (newline op)))

;(trace put)

(define (do-startup-stuff op)
  (put (format "NICK ~a" (*my-nick*)) op)
  (put (format "USER ~a ~a ~a :~a, ~a"
               (getenv "USER")
               "unknown-host"
               (*irc-server-name*)
               *client-name*
               (*client-version*)) op)  )

;; these are running (or possibly dead) tasks.  Perhaps this table
;; could be combined with the above.  The values would be a bunch of
;; startup stuff, plus the actual task, which isn't running yet; then
;; at channel-join time I'd merely start the task.
(define *tasks-by-channel*  (make-hash-table 'equal))

;; string? output-port? -> void

(define (vfilter proc seq)
  (filter (lambda (thing)
            (let ((rv (proc thing)))
              (vtprintf "filtering ~s: ~s~%" thing rv)
              rv))
          seq))

;; given a line of text (presumably from the IRC server), spew a
;; response to the output port, and perhaps do all sorts of evil
;; untestable kludgy side-effects (like starting a thread that will
;; eventually spew more stuff to the output port)
(define (respond line op)

  (define (do-something-clever
           message-tokens               ;what they said
           requestor                    ;who said it
           channel-name                 ;where they said it
           was-private?                 ;how they said it
           CTCP-message?                ;was it a CTCP?
           )

    ;; maybe throw away some of the initial tokens, since they're
    ;; not interesting.

    ;; was-private?         CTCP-message?           which tokens
    ;; #f                   #f                      first two (channel name, my name)
    ;; #f                   #t                      none
    ;; #t                   #f                      first one (my name)
    ;; #t                   #t                      none
    (when (not CTCP-message?)
      (set! message-tokens
            ((if was-private? cdr cddr)
             message-tokens)))
    (cond
     (CTCP-message?
      =>
      (lambda (msg)
        (vprintf "Ooh, ~s is a CTCP message yielding ~s~%"
                 message-tokens
                 msg)
        (cond
         ((string=? "VERSION" (first message-tokens))
          (put (format "NOTICE ~a :\001VERSION ~a (offby1@blarg.net):~a:~a\001"
                       requestor
                       *client-name*
                       *client-version-number*
                       *client-environment*) op)))))
     (else
      (let ((response-body
             (cond
              ;; if the requestor seems to be a bot, don't respond normally.
              ((regexp-match (pregexp "bot[^[:space:][:alnum:]]*$") requestor)
               "\u0001ACTION holds his tongue.\u0001")

              ((and (string-ci=? "seen" (first message-tokens))
                    (< 1 (length message-tokens)))
               (let* ((nick (second message-tokens))
                      (times-by-nick (hash-table-get
                                      times-by-nick-by-channel
                                      channel-name
                                      (make-hash-table 'equal)))
                      (data (hash-table-get times-by-nick nick #f)))
                 (vprintf "Sought key ~s in times-by-nick; got ~s~%"
                          nick data)
                 (cond
                  ((not data)
                   (format "I haven't seen ~a in ~a" nick channel-name))
                  ((utterance-action? data)
                   (format "~a's last action in ~a was at ~a: ~a ~a"
                           nick
                           channel-name
                           (zdate (utterance-when data))
                           nick
                           (utterance-what data)))
                  (else
                   (format "~a last spoke in ~a at ~a, saying \"~a\""
                           nick
                           channel-name
                           (zdate (utterance-when data))
                           (utterance-what data))))))

              (else
               "\u0001ACTION is at a loss for words.\u0001"))))

        (put (format "PRIVMSG ~a :~a"
                     (if was-private? requestor channel-name)
                     response-body) op)))))

  (set! p* (lambda (str)
             (put str op)))

  (let ((line (substring line 0 (min 510 (string-length line)))))
    (let-values (((prefix command params)
                  (parse-message line)))
      (let ((prefix (parse-prefix prefix)))
        (vtprintf "<= ~s -> prefix ~s; command ~s params ~s ~%"
                  line
                  prefix
                  command
                  params)

        (let ((command-number (and (regexp-match (pregexp "^[[:digit:]]{3}$") command )
                                   (string->number command)))
              (command-symbol (and (regexp-match (pregexp "^[[:alpha:]]+$") command)
                                   (string->symbol command))))
          (case command-number
            ((001)
             (for-each (lambda (ch)
                         (put (format "JOIN ~a" ch) op))
                       (*initial-channel-names*)))
            ((353)
             (vtprintf "Got the 353~%")
             ;; start up whatever tasks pertain to this channel, if we
             ;; haven't already
             (let ((tokens (split params)))
               (if (< (length tokens) 3)
                   (vtprintf "Server is on drugs: there should be three tokens here: ~s~%" params)
                 ;; response to "NAMES".  This implies we've joined a
                 ;; channel (or issued a NAMES command ourselves,
                 ;; which we don't do, afaik)
                 (let ((channel (third tokens)))
                   (vtprintf "353: tokens ~s; channel ~s~%"
                             tokens channel)
                   (for-each
                    (lambda (t)
                      (vtprintf "Unsuspending task ~s~%" (task-name-symbol t))
                      (task-unsuspend t))
                    (hash-table-get *tasks-by-channel* channel '()))))))
            ((433)
             (vtprintf "Gaah!!  One of those \"nick already in use\" messages!~%")
             (vtprintf "I don't know how to deal with that~%")
             (vtprintf "Just start me up again and provide a different nick on the command line, I guess :-|~%")))

          (case command-symbol
            ((PRIVMSG)
             (let* ((CTCP-message (cond
                                   ((regexp-match #rx"\u0001(.*)\u0001$" params)
                                    => second)
                                   (else #f)))
                    (tokens
                     (cond
                      (CTCP-message => split)
                      (else (split params))))
                    (tokens
                     ;; I can't remember why I'm removing this colon
                     (if (<= 2 (length tokens))
                         (cons (first tokens)
                               (cons (regexp-replace
                                      #rx"^:"
                                      (second tokens)
                                      "")
                                     (cddr tokens)))
                       tokens))
                    (destination (car (split params)))
                    (dest-is-channel? (regexp-match #rx"^#" destination))
                    (source (car prefix)))

               (when dest-is-channel?
                 (for-each postpone (hash-table-get *tasks-by-channel* destination '()))

                 (let* ((times-by-nick (hash-table-get
                                        times-by-nick-by-channel
                                        destination
                                        (lambda ()
                                          (make-hash-table 'equal))))
                        (utterance (make-utterance
                                    (seconds->date (current-seconds))
                                    (string-join (cdr tokens))
                                    (not (not CTCP-message)))))
                   (hash-table-put! times-by-nick source utterance)
                   (hash-table-put! times-by-nick-by-channel destination times-by-nick)))

               (unless (*passive?*)

                 (cond
                  ;; private message to us.
                  ((equal? (*my-nick*) destination)
                   (do-something-clever
                    tokens
                    source
                    destination
                    #t
                    CTCP-message))

                  ;; someone said something to the whole channel.
                  ((and dest-is-channel?
                        (< 1 (length tokens)))

                   ;; ... but prefixed it with my nick.
                   (when (regexp-match
                          (regexp
                           (string-append
                            "^"
                            (pregexp-quote (*my-nick*))
                            "[:,]"))
                          (cadr tokens))
                     (cond
                      ((string-ci=? "quote" (third tokens))

                       (for-each do-it-now!
                                 (vfilter (lambda (task)
                                            (eq? (task-name-symbol task) 'quote-spewer-task))
                                          (hash-table-get *tasks-by-channel* destination '()))))

                      ((string-ci=? "news" (third tokens))

                       (for-each do-it-now!
                                 (vfilter (lambda (task)
                                            (eq? (task-name-symbol task) 'headline-spewer-task))
                                          (hash-table-get *tasks-by-channel* destination '()))))
                      (else
                       (do-something-clever
                        tokens
                        source
                        destination
                        #f
                        CTCP-message))))))))
             )
            ((NOTICE)
             (when (regexp-match #rx"No identd \\(auth\\) response" params)
               (fprintf
                (current-error-port)
                "This is one of those servers that wants us to run identd.  Be patient; it'll take two minutes to connect.~%")
               ))
            ((PING)
             (put (format "PONG ~a" params) op))))))))
;(trace respond)
(define times-by-nick-by-channel (make-hash-table 'equal))
)