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
               first second third fourth fifth)
         (only (lib "13.ss" "srfi")
               string-tokenize
               )
         (only (lib "14.ss" "srfi")
               char-set
               char-set-complement
               char-set:whitespace
               )
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (only "globals.ss"
               register-version-string
               *my-nick*)
         (only (planet "assert.ss" ("offby1" "offby1.plt")) check-type))

(register-version-string "$Id$")
(define-struct (exn:fail:irc-parse exn:fail:contract) (string original-exception))
(define-struct message (prefix command params) (make-inspector))

;; turn "NOTICE", e.g., into 'NOTICE
(define (public-message-command x)
  (read-from-string (message-command x)))

(define-struct (PRIVMSG message)
  (speaker destination approximate-recipient text text-words)
  (make-inspector))
(define-struct (CTCP PRIVMSG) (req/extended-data) (make-inspector))
(define-struct (ACTION CTCP) () (make-inspector))
(define-struct (VERSION CTCP) () (make-inspector))
(define-struct (SOURCE CTCP) () (make-inspector))
;; (trace make-message)
;; (trace make-PRIVMSG)
;; (trace make-CTCP)
(define (PRIVMSG-is-for-channel? m)
  (and (PRIVMSG? m)
       (regexp-match #rx"^#" (PRIVMSG-destination m))))
;; (trace PRIVMSG-is-for-channel?)
;; (trace PRIVMSG-destination)
(define (parse-irc-message string)
  (with-handlers
      ([exn:fail:contract?
        (lambda (e)
          (raise (make-exn:fail:irc-parse
                  (exn-message e)
                  (exn-continuation-marks e)
                  string e)))])
    (let ((prefix #f)
          (sans-prefix string))
      (when (char=? #\: (string-ref string 0))
        (let ((m (regexp-match #rx"^:(.*?) (.*)$" string)))
          (set! prefix (second m))
          (set! sans-prefix (third m))))
      (let* ((m (regexp-match #rx"^(.*?) (.*)$" sans-prefix))
             (command (second m))
             (sans-command (third m)))
        (let* ((m (regexp-match #rx"((.*?) )?(:(.*))?$" sans-command))
               (middle-params (string-tokenize
                               (or (second m) "")
                               (char-set-complement (char-set #\space))))
               (trailing-parameter (let ((t (fifth m)))
                                     (and t
                                          (positive? (string-length t))
                                          t)))
               (addressee
                (and
                 trailing-parameter
                 (let ((tp-tokens (string-tokenize
                                   trailing-parameter
                                   (char-set-complement (char-set #\space)))))
                   (and (not (null? tp-tokens))
                        (let ((fw (first tp-tokens))
                              (trailing-delimiter-rx #rx"[:,]$"))
                          (and
                           (regexp-match
                            trailing-delimiter-rx
                            fw)
                           (regexp-replace
                            trailing-delimiter-rx
                            fw
                            "")))))))
               )
          (if (string=? "PRIVMSG" command)
              (let* ((ctcp-match (regexp-match
                                  (pregexp "^\u0001([[:alpha:]]+) ?(.*)\u0001$")
                                  trailing-parameter))
                     (text (if ctcp-match (third ctcp-match)
                             trailing-parameter))
                     (req/x (and ctcp-match (second ctcp-match)))
                     (prefix-match (regexp-match "^(.*)!(.*)@(.*)$" prefix)))
                (apply
                 (cond
                  ((not ctcp-match)         make-PRIVMSG)
                  ((equal? req/x "ACTION" ) make-ACTION)
                  ((equal? req/x "VERSION") make-VERSION)
                  ((equal? req/x "SOURCE" ) make-SOURCE)
                  (else make-CTCP))
                 prefix command (append middle-params (list text))
                 (second prefix-match)
                 (first middle-params)
                 addressee
                 text
                 (string-tokenize
                  text
                  (char-set-complement char-set:whitespace))
                 (if ctcp-match (list (first ctcp-match)) '())))
            (make-message
             prefix command
             (append
              middle-params
              (if trailing-parameter
                  (list trailing-parameter)
                '()))
             )))))))

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
    "no trailing"
    ":foo bar baz :"                         "foo" "bar" '("baz"))
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
    ":ChanServ!ChanServ@services. MODE #cinema +tc "
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
    "no trailing params"
    (message-params (parse-irc-message "COMMAND foo bar baz :"))
    (list "foo" "bar" "baz"))
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
                   "poo")
     (check-equal? (PRIVMSG-text (parse-irc-message ":X!X@Y PRIVMSG poo poo :platter puss"))
                   "platter puss")
     (check-equal? (PRIVMSG-speaker (parse-irc-message ":fsbot!n=user@batfish.pepperfish.net PRIVMSG #emacs :yow!"))
                   "fsbot")
     )
    (test-suite
     "CTCP"
     (test-false
      "rejects non-actions"
      (ACTION?
       (parse-irc-message ":X!X@Y PRIVMSG #playroom :\u0001UNDERWEAR eats cornflakes\u0001")))
     (test-case
      "recognizes and parses ACTION"
      (let ((m (parse-irc-message ":X!X@Y PRIVMSG #playroom :\u0001ACTION eats cornflakes\u0001")))
        (check-pred PRIVMSG? m)
        (check-pred CTCP? m)
        (check-equal? (PRIVMSG-text m) "eats cornflakes")))
     (test-case
      "recognizes VERSION"
      (check-pred
       VERSION?
       (parse-irc-message ":X!X@Y PRIVMSG #playroom :\u0001VERSION\u0001")))

     (test-case
      "recognizes SOURCE"
      (check-pred
       SOURCE?
       (parse-irc-message ":X!X@Y PRIVMSG #playroom :\u0001SOURCE\u0001"))))

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
   (test-suite
    "gists"
    (test-not-false "for-us"  (for-us? (parse-irc-message (format ":x!y@z PRIVMSG ~a :yow" (*my-nick*)))))
    (test-not-false "for-us"  (for-us? (parse-irc-message (format ":x!y@z PRIVMSG #some-chan :~a: yow" (*my-nick*)))))
    (test-false     "for-us"  (for-us? (parse-irc-message (format ":x!y@z PRIVMSG x~ax :yow" (*my-nick*)))))
    (test-false     "for-us"  (for-us? (parse-irc-message (format ":x!y@z PRIVMSG #some-chan :x~ax: yow" (*my-nick*)))))

    (test-not-false
     "gist-equal?"
     (gist-equal?
      "yow"
      (parse-irc-message (format ":x!y@z PRIVMSG ~a :yow" (*my-nick*)))))

    (test-not-false
     "gist-equal?"
     (gist-equal?
      "yow"
      (parse-irc-message (format ":x!y@z PRIVMSG #ch-ch-ch-changes :~a, yow" (*my-nick*))))))))

(provide (all-defined-except message-command)
         (rename public-message-command message-command))
)
