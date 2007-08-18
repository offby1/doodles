#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
  exec  mzscheme                                        \
  -M errortrace                                         \
  --no-init-file                                        \
  --mute-banner                                         \
  --version                                             \
  --require "$0"                                        \
  -p "text-ui.ss" "schematics" "schemeunit.plt"         \
  -e "(exit (or #f (test/text-ui parse-tests 'verbose)))"
|#
(module parse mzscheme
(require (lib "trace.ss")
         (only (lib "string.ss") read-from-string)
         (only (planet "port.ss" ("schematics" "port.plt" 1 0))
               port->string-list)
         (only (lib "1.ss" "srfi")
               any
               first second third fourth fifth)
         (only (lib "13.ss" "srfi")
               string-tokenize
               )
         (only (lib "14.ss" "srfi")
               char-set
               char-set-complement
               char-set:whitespace
               )
         (only (lib "misc.ss""swindle") regexp-case)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only "globals.ss"
               register-version-string
               *my-nick*)
         (only (planet "assert.ss" ("offby1" "offby1.plt")) check-type))

(register-version-string "$Id$")

(define (make-sub-struct x sub-constructor . more-args)
  (apply sub-constructor
         (append (cdr (vector->list (struct->vector x)))
                 more-args)))

(define-struct (exn:fail:irc-parse exn:fail) (string))
(define-struct message (prefix command params) #f)

;; turn "NOTICE", e.g., into 'NOTICE
(define (public-message-command x)
  (read-from-string (message-command x)))

(define-struct (PRIVMSG message)
  (speaker destination approximate-recipient text text-words)
  #f)
(define-struct (CTCP PRIVMSG) (req/extended-data) #f)
(define-struct (ACTION CTCP) () #f)
(define-struct (VERSION CTCP) () #f)
(define-struct (SOURCE CTCP) () #f)
;; (trace make-message)
;; (trace make-PRIVMSG)
;; (trace make-CTCP)
(define (PRIVMSG-is-for-channel? m)
  (and (PRIVMSG? m)
       (any (lambda (d)
              (regexp-match #rx"^#" d))
            (PRIVMSG-destination m))))
;; (trace PRIVMSG-is-for-channel?)
;; (trace PRIVMSG-destination)

(define (parse-irc-message string)
  (let ((m ;; http://tools.ietf.org/html/rfc1459#page-8
         (regexp-case
          string
          ((#rx"^(?::(.*?) )?(.*)$" prefix sans-prefix)
           (regexp-case
            sans-prefix
            ((#rx"^(.*?)( .*)$" command param-string)

             (let loop ((params '())
                        (param-string param-string))
               (printf "loop: ~s; ~s~%" params param-string)
               (if (zero? (string-length param-string))
                   (make-message prefix command (reverse params))
                 (regexp-case
                  param-string
                  ((#rx"^ :(.*)" trail)
                   (printf "trail is ~s~%" trail)
                   (make-message prefix command (reverse (cons trail params))))
                  (((pregexp "^ ([^[:space:]]+)") one)
                   (printf "one is ~s~%" one)
                   (loop (cons one params)
                         (substring param-string (string-length match)))))))))))))

    (when (not (message? m))
      (raise (make-exn:fail:irc-parse
              "Can't parse string from server"
              (current-continuation-marks)
              string)))
    (or (maybe-make-PRIVMSG m)
        m)))

(trace parse-irc-message)

(define (maybe-make-PRIVMSG m)
  (define speaker
    (and (message-prefix m)
         (regexp-case
          (message-prefix m)
          ((#rx"(.*)!(.*)@(.*)" nick user host)
           nick))))

  (and (string=? "PRIVMSG" (message-command m))
       (let ((receivers (string-tokenize
                         (first (message-params m))
                         (char-set-complement (char-set #\,))))
             (text (second (message-params m))))
         (let ((text-tokens
                (string-tokenize
                 text
                 (char-set-complement char-set:whitespace))))
           (make-sub-struct
            m
            make-PRIVMSG
            speaker
            receivers
            (and (not (null? text-tokens))
                 (regexp-case
                  (car text-tokens)
                  ((pregexp "^([[:alnum:]]+)[:,]")
                   => (lambda args (second args)))
                  (else #f)))
            text
            text-tokens)))))
(trace maybe-make-PRIVMSG)
(define (for-us? message)
  (check-type 'for-us? message? message)
  (and
   (PRIVMSG? message)
   (equal? (*my-nick*)
           ((if (PRIVMSG-is-for-channel? message)
                PRIVMSG-approximate-recipient
              PRIVMSG-destination) message))))

(define (gist-for-us message)
  (check-type 'gist-for-us message? message)
  (let ((relevant-word
         (and (for-us? message)
              (cond
               ((and (PRIVMSG-is-for-channel? message)
                     (< 1 (length (PRIVMSG-text-words message))))
                (second (PRIVMSG-text-words message)))
               ((and (not (PRIVMSG-is-for-channel? message))
                     (< 0 (length (PRIVMSG-text-words message))))
                (first (PRIVMSG-text-words message)))
               (else
                #f))
              )))
    ;; trim trailing punctuation
    (and relevant-word
         (regexp-replace (pregexp "[^[:alpha:]]+$") relevant-word ""))))

(define (gist-equal? str message)
  (check-type 'gist-equal? message? message)
  (equal? str (gist-for-us message)))

;(trace parse-irc-message)


(define-shortcut (test-parse input pref cmd params)
  (let ((m (parse-irc-message input)))
    (check-equal? (message-prefix  m) pref   (format "prefix of ~s"  input))
    (check-equal? (message-command m) cmd    (format "command of ~s" input))
    (check-equal? (message-params  m) params (format "params of ~s"  input))))

(define parse-tests

  (test-suite
   "parsing"
   (test-case
    "barfs on malformed data from server"
    (check-exn
     exn:fail:irc-parse? (lambda () (parse-irc-message ":foo ")))
    (check-exn
     exn:fail:irc-parse? (lambda () (parse-irc-message ":foo :"))))

   (test-parse
    "empty trailing"
    ":foo bar baz :"                         "foo" "bar" '("baz" ""))
   (test-parse
    "trailing"
    ":foo bar baz :params go here"          "foo" "bar" '("baz" "params go here"))
   (test-parse
    "NOTICE"
    ":localhost. NOTICE you :all suck"
    "localhost."
    "NOTICE"
    '("you" "all suck"))
   (test-parse
    "PRIVMSG"
    ":foo!foo@localhost. PRIVMSG #emacs :e1f: you all suck"
    "foo!foo@localhost."
    "PRIVMSG"
    '("#emacs" "e1f: you all suck"))
   (test-parse
    "MODE"
    ":ChanServ!ChanServ@services. MODE #cinema +tc"
    "ChanServ!ChanServ@services."
    "MODE"
    '("#cinema" "+tc"))
   (test-equal?
    "prefix"
    (message-prefix (parse-irc-message ":zip zap zop :snot"))
    "zip")
   (test-false
    "missing prefix"
    (message-prefix (parse-irc-message "NOTICE All Apple fanbois will be taken out back")))
   (test-equal?
    "command"
    (message-command (parse-irc-message "NOTICE All Apple fanbois will be taken out back"))
    "NOTICE")
   (test-equal?
    "trailing params (not ust trailing)"
    (message-params (parse-irc-message "COMMAND poo poo :platter puss"))
    (list "poo" "poo" "platter puss"))
   (test-suite
    "PRIVMSGs"
    (test-not-exn
     "No puke on a single-space"
     (lambda ()
       (parse-irc-message ":fledermaus!n=vivek@pdpc/supporter/active/fledermaus PRIVMSG #emacs : ")))
    (test-false
     "average command isn't a PRIVMSG"
     (PRIVMSG? (parse-irc-message "COMMAND poo poo :platter puss")))
    (test-pred
     "PRIVMSGs are indeed PRIVMSGs"
     PRIVMSG?
     (parse-irc-message ":X!X@Y PRIVMSG poo poo :platter puss"))
    (test-case
     "PRIVMSGs get properly parsed"
     (check-equal? (PRIVMSG-destination (parse-irc-message ":X!X@Y PRIVMSG poo poo :platter puss"))
                   '("poo"))
     (check-equal? (PRIVMSG-text (parse-irc-message ":X!X@Y PRIVMSG poopoo :platter puss"))
                   "platter puss")
     (check-equal? (PRIVMSG-speaker (parse-irc-message ":fsbot!n=user@batfish.pepperfish.net PRIVMSG #emacs :yow!"))
                   "fsbot")
     )
;;     (test-suite
;;      "CTCP"
;;      (test-false
;;       "rejects non-actions"
;;       (ACTION?
;;        (parse-irc-message ":X!X@Y PRIVMSG #playroom :\u0001UNDERWEAR eats cornflakes\u0001")))
;;      (test-case
;;       "recognizes and parses ACTION"
;;       (let ((m (parse-irc-message ":X!X@Y PRIVMSG #playroom :\u0001ACTION eats cornflakes\u0001")))
;;         (check-pred PRIVMSG? m)
;;         (check-pred CTCP? m)
;;         (check-equal? (PRIVMSG-text m) "eats cornflakes")))
;;      (test-case
;;       "recognizes VERSION"
;;       (check-pred
;;        VERSION?
;;        (parse-irc-message ":X!X@Y PRIVMSG #playroom :\u0001VERSION\u0001")))

;;      (test-case
;;       "recognizes SOURCE"
;;       (check-pred
;;        SOURCE?
;;        (parse-irc-message ":X!X@Y PRIVMSG #playroom :\u0001SOURCE\u0001"))))

    (test-case
     "channel versus truly private message"
     (check-pred
      PRIVMSG-is-for-channel?
      (parse-irc-message ":X!X@Y PRIVMSG #playroom :\u0001ACTION eats cornflakes\u0001"))
     (check-false
      (PRIVMSG-is-for-channel?
       (parse-irc-message ":X!X@Y PRIVMSG sam :\u0001ACTION eats cornflakes\u0001"))))

    (test-case
     "approximate recipient"
     (check-false
      (PRIVMSG-approximate-recipient
       (parse-irc-message ":X!X@Y PRIVMSG sam :\u0001ACTION eats cornflakes\u0001")))
     (check-false
      (PRIVMSG-approximate-recipient
       (parse-irc-message ":X!X@Y PRIVMSG sam :well I think you smell")))
     (check-equal?
      (PRIVMSG-approximate-recipient
       (parse-irc-message ":X!X@Y PRIVMSG sam :well, I think you smell"))
      "well"))

    )
;;    (test-suite
;;     "gists"
;;     (test-not-false "for-us"  (for-us? (parse-irc-message (format ":x!y@z PRIVMSG ~a :yow" (*my-nick*)))))
;;     (test-not-false "for-us"  (for-us? (parse-irc-message (format ":x!y@z PRIVMSG #some-chan :~a: yow" (*my-nick*)))))
;;     (test-false     "for-us"  (for-us? (parse-irc-message (format ":x!y@z PRIVMSG x~ax :yow" (*my-nick*)))))
;;     (test-false     "for-us"  (for-us? (parse-irc-message (format ":x!y@z PRIVMSG #some-chan :x~ax: yow" (*my-nick*)))))

;;     (test-not-false
;;      "gist-equal?"
;;      (gist-equal?
;;       "yow"
;;       (parse-irc-message (format ":x!y@z PRIVMSG ~a :yow" (*my-nick*)))))

;;     (test-not-false
;;      "gist-equal?"
;;      (gist-equal?
;;       "yow"
;;       (parse-irc-message (format ":x!y@z PRIVMSG #ch-ch-ch-changes :~a, yow" (*my-nick*))))))
   ))

(provide (all-defined-except message-command)
         (rename public-message-command message-command)))

