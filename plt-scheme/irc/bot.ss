#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; http://tools.ietf.org/html/rfc1459

(module bot mzscheme
(require (lib "async-channel.ss")
         (lib "trace.ss")
         (only (lib "pregexp.ss") pregexp-quote)
         (only (planet "rfc3339.ss" ("neil" "rfc3339.plt"))
               rfc3339-string->srfi19-date/constructor)
         (only (lib "url.ss" "net")
               get-pure-port
               string->url)
         (only (lib "1.ss" "srfi")
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
         (rename (lib "19.ss" "srfi") 19:make-date make-date)
         (only (lib "19.ss" "srfi")
               add-duration
               current-date
               date->time-utc
               make-time
               time-duration
               time-utc->date
               time>?
               )
         (only (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
               sxpath)
         (only (planet "zdate.ss" ("offby1" "offby1.plt")) zdate)
         "parse-message.ss"
         "globals.ss"
         "jordanb.ss"
         "planet-emacsen.ss"
         "vprintf.ss"
         "../web/quote-of-the-day.ss")
(provide
 *atom-timestamp-file-name*
 do-startup-stuff
 kill-all-tasks
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

;; during normal operation we want our bot to act randomly.  But when
;; we're testing it, we want it to act predictably.  Thus we have a
;; parameter called *random?* which determines which way it acts.

;; I know that I could start it with a known seed when I test, but for
;; reasons I can't articulate, that seems less pleasant than simply
;; having "rnd" always return 0.
(define (rnd . args)
  (if (not (*random?*))
      0
    (apply random args)))

(define (random-choice seq)
  (list-ref seq (rnd (length seq))))

;; Calls THUNK every SECONDS seconds.  Calling the return value with
;; the symbol POSTPONE postpones the next call (i.e., it resets the
;; timer).  Calling the return value with any other value kills the
;; task permanently.

;; TODO -- maybe make a wrapper for this called
;; "do-when-channel-idle", which saves the task someplace out of the
;; way, monitors the named channel, and cancels the task as needed.
;; Right now those things are being done in "respond".
;; sit around and wait a while, then do the thunk, then start over.

;; we'll cut the wait short, and do the thunk, if someone shoves an #f
;; at us.

;; we'll cut the wait short, and _not_ do the thunk, if someone shoves
;; 'POSTPONE at us.

;; we'll go away entirely if we receive any other value.
(define (do-in-loop seconds thunk)
  (let* ((c (make-channel))
         (t (thread (lambda ()
                      (let loop ()
                        (let ((reason (sync/timeout seconds c)))
                          ;(printf "sync/timeout returned ~s~%" reason)
                          (when (or (not reason) ;timed out
                                    (not (channel-get c)))
                            (thunk)))
                        (loop)
                        )))))
    (lambda (command)
      (if (memq command '(#f postpone POSTPONE))
          (channel-put c command)
        (kill-thread t)))))

(define *jordanb-quote-tasks-by-channel* (make-hash-table 'equal))
(define *planet-emacs-task* #f)

;; for testing
(define (kill-all-tasks)
  (for-each (lambda (thing)
              (when (thread? thing) (kill-thread thing)))
            (cons *planet-emacs-task* (map cdr (hash-table-map *jordanb-quote-tasks-by-channel* cons)))))

(define-struct utterance (when what action?) (make-inspector))
(define (put str op)
  (let ((str (substring str 0 (min 510 (string-length str)))))
    (vtprintf "=> ~s~%" str)
    (display str op)
    (newline op)
    (flush-output op)))


(define (do-startup-stuff op)
  (put (format "NICK ~a" (*my-nick*)) op)
  (put (format "USER ~a ~a ~a :~a, ~a"
               (getenv "USER")
               "unknown-host"
               (*irc-server-name*)
               *client-name*
               (*client-version*)) op)  )

(define *atom-timestamp-file-name* "timestamp")

(define (make-bounded-queue size)
  (let ((the-queue '()))
    (define (bounded-queue . args)
      (cond
       ((null? args)
        the-queue)
       ((null? (cdr args))
        (when (= (length the-queue)
                 size)
          (set! the-queue
                (take the-queue
                      (sub1 size))))
        (set! the-queue (cons (car args)
                              the-queue)))
       (else
        (error 'bounded-queue "I don't know what to do with" args))))
    ;;(trace bounded-queue)
    bounded-queue))

(define *some-recent-entries*
  (make-bounded-queue
   ;; choose a number that's small enough so that this many news items
   ;; can fit in one IRC message ... which is something around 500
   ;; bytes
   3))

;; string? output-port? -> void

;; now that I think about it, there's no good reason this couldn't
;; just be string? -> (listof string?)
(define ( respond line op)

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
     ((and (hash-table-get *jordanb-quote-tasks-by-channel* channel-name #f)
           (string-ci=? "shaddap" (first message-tokens)))
      ((hash-table-get *jordanb-quote-tasks-by-channel* channel-name) 'kill)
      ;; TODO -- maybe now do hash-table-remove! so that the next
      ;; time we see a 353, we recreate the task.
      )
     (else
      (let ((response-body
             (cond
              ;; if the requestor seems to be a bot, don't respond normally.
              ((regexp-match (pregexp "bot[^[:space:][:alnum:]]*$") requestor)
               "\u0001ACTION holds his tongue.\u0001")

              ((string-ci=? "news" (first message-tokens))
               ;; TODO -- maybe send an #f to the task
               (apply string-append
                      (map entry->string (*some-recent-entries*))))
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

              ((regexp-match #rx"(?i:^quote)( .*$)?" (first message-tokens))
               (let try-again ()
                 (let ((r  (rnd 100)))
                   ;; we special-case zero for ease of testing.
                   (cond ((zero? r)
                          "I've laid out in my will that my heirs should continue working on my .emacs -- johnw")
                         ((< r 91)
                          ;; TODO: here's a cute idea -- if
                          ;; requestor appears to be jordanb
                          ;; himself, return something utterly
                          ;; unlike the usual jordanb quote --
                          ;; something saccharine and Hallmark-y
                          (one-jordanb-quote))
                         (else
                          (with-handlers
                              ((exn:fail:network?
                                (lambda (e)
                                  (vtprintf "Warning: quote-of-the-day yielded an exception: ~a~%"
                                            (exn-message e))
                                  (try-again))))
                            (let ((p (random-choice (quotes-of-the-day))))
                              (string-append (car p)
                                             "  --"
                                             (cdr p)))))))))
              (else
               "\u0001ACTION is at a loss for words.\u0001"))))

        (put (format "PRIVMSG ~a :~a"
                     (if was-private? requestor channel-name)
                     response-body) op)))))

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
             (let ((tokens (split params)))
               (if (< (length tokens) 3)
                   (vtprintf "Server is on drugs: there should be three tokens here: ~s~%" params)
                 ;; response to "NAMES".  This implies we've joined a
                 ;; channel (or issued a NAMES command ourselves,
                 ;; which we don't do, afaik)
                 (let ((channel (third tokens)))

                   (when (or
                          (string=? channel "#bots")
                          (string=? channel "#emacs"))
                     (when
                         ;; I wonder ... would it be better if I
                         ;; killed any existing task, and then
                         ;; replaced it with a new one?
                         (not (hash-table-get *jordanb-quote-tasks-by-channel* channel #f))
                       (hash-table-put!
                        *jordanb-quote-tasks-by-channel*
                        channel
                        (do-in-loop
                         (*jordanb-quote-interval*)
                         (lambda ()
                           (put (format "PRIVMSG ~a :~a"
                                        channel
                                        (one-jordanb-quote)) op)))))

                     (when (not *planet-emacs-task*)
                       (set! *planet-emacs-task*
                             (let ((atom-feed (queue-of-entries
                                               #:whence
                                               (and (*use-real-atom-feed?*)
                                                    (lambda ()
                                                      (vtprintf "SNARFING REAL DATA FROM WEB!!!!!!!~%")
                                                      (get-pure-port
                                                       (string->url "http://planet.emacsen.org/atom.xml")
                                                       (list)))
                                                    )))
                                   (number-spewed 0)
                                   (time-of-latest-spewed-entry
                                    (date->time-utc
                                     (rfc3339-string->srfi19-date/constructor
                                      (or (and (file-exists? *atom-timestamp-file-name*)
                                               (call-with-input-file *atom-timestamp-file-name* read))
                                          "2000-00-00T00:00:00+00:00")
                                      19:make-date))))

                               (do-in-loop
                                ;; choosing the "correct" interval
                                ;; here is subtle.  Ideally the
                                ;; interval would have the property
                                ;; that the channel goes silent for
                                ;; this long just as often as someone
                                ;; posts a blog to planet emacs --
                                ;; that way we consume items from the
                                ;; channel at the same rate that
                                ;; queue-of-entries-since produces
                                ;; them.  Failing that, it's perhaps
                                ;; best for this number to be a bit
                                ;; smaller than that idea, so that
                                ;; this task finds nothing new
                                ;; occasionally.
                                (*planet-task-spew-interval*)
                                (lambda ()
                                  (let ((datum (async-channel-try-get atom-feed)))
                                    (vtprintf "Consumer thread: Just got ~s from our atom feed~%" datum)
                                    (when datum
                                      (*some-recent-entries* datum)

                                      ;; spew any _new_ entries that we
                                      ;; haven't already spewed ... but
                                      ;; also spew the single newest entry
                                      ;; even if it's kind of old.
                                      (if (time>?
                                           (entry-timestamp datum)
                                           time-of-latest-spewed-entry)
                                          (begin
                                            (put (format "PRIVMSG #emacs :~a"
                                                         (entry->string datum)) op)
                                            (set! number-spewed (add1 number-spewed))
                                            (when (time>?
                                                   (entry-timestamp datum)
                                                   time-of-latest-spewed-entry)
                                              (set! time-of-latest-spewed-entry
                                                    (entry-timestamp datum))
                                              (call-with-output-file
                                                  *atom-timestamp-file-name*
                                                (lambda (op)
                                                  (write
                                                   (zdate
                                                    (time-utc->date time-of-latest-spewed-entry))
                                                   op))
                                                'truncate/replace)))
                                        (vtprintf "Consumer thread: Nothing new on planet emacs~%"))))
                                  ))))))

                   ))))
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
                 (for-each
                  (lambda (task)
                    (when task
                      (task 'postpone)))
                  (list
                   (hash-table-get *jordanb-quote-tasks-by-channel* destination #f)
                   *planet-emacs-task*))

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
                     (do-something-clever
                      tokens
                      source
                      destination
                      #f
                      CTCP-message))))))
             )
            ((NOTICE)
             (when (regexp-match #rx"No identd \\(auth\\) response" params)
               (fprintf
                (current-error-port)
                "This is one of those servers that wants us to run identd.  Be patient; it'll take two minutes to connect.~%")
               ))
            ((PING)
             (put (format "PONG ~a" params) op))))))))

(define times-by-nick-by-channel (make-hash-table 'equal))
)