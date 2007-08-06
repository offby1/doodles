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
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

(define-struct message (prefix command params) (make-inspector))
(define (public-message-command x)
  (read-from-string (message-command x)))

(define-struct (PRIVMSG message) (destination text text-words) (make-inspector))
(define-struct (ACTION PRIVMSG) () (make-inspector))
;(trace make-message)
;(trace make-PRIVMSG)
;(trace make-ACTION)
(define (parse-irc-message string)
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
                                        t))))
        (if (string=? "PRIVMSG" command)
            (let* ((m (regexp-match "^\u0001ACTION (.*)\u0001$" trailing-parameter))
                   (text (if m (second m)
                           trailing-parameter)))
              ((if m make-ACTION make-PRIVMSG)
               prefix command (append middle-params (list text))
               (first middle-params)
               text
               (string-tokenize
                text
                (char-set-complement char-set:whitespace))))
          (make-message
           prefix command
           (append
            middle-params
            (if trailing-parameter
                (list trailing-parameter)
              '()))))))))
;(trace parse-irc-message)


(define (test-parse input pref cmd params)
  (test-case
   "yow"
   (let ((m (parse-irc-message input)))
     (check-equal? (message-prefix  m) pref   (format "prefix of ~s"  input))
     (check-equal? (message-command m) cmd    (format "command of ~s" input))
     (check-equal? (message-params  m) params (format "params of ~s"  input)))))

(define parse-tests

  (test-suite
   "parsing"
   (test-case
    "barfs on malformed data from server"
    (check-exn
     exn:fail:contract? (lambda () (parse-irc-message ":foo ")))
    (check-exn
     exn:fail:contract? (lambda () (parse-irc-message ":foo :"))))

   (test-parse ":foo bar baz :"                         "foo" "bar" '("baz"))
   (test-parse ":foo bar baz :params go here"          "foo" "bar" '("baz" "params go here"))
   (test-parse ":localhost. NOTICE you :all suck"
               "localhost."
               "NOTICE"
               '("you" "all suck"))
   (test-parse ":localhost. PRIVMSG #emacs :e1f: you all suck"
               "localhost."
               "PRIVMSG"
               '("#emacs" "e1f: you all suck"))
   (test-parse
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
    (test-false
     "average command isn't a PRIVMSG"
     (PRIVMSG? (parse-irc-message "COMMAND poo poo :platter puss")))
    (test-pred
     "PRIVMSGs are indeed PRIVMSGs"
     PRIVMSG?
     (parse-irc-message "PRIVMSG poo poo :platter puss"))
    (test-case
     "PRIVMSGs get properly parsed"
     (check-equal? (PRIVMSG-destination (parse-irc-message "PRIVMSG poo poo :platter puss"))
                   "poo")
     (check-equal? (PRIVMSG-text (parse-irc-message "PRIVMSG poo poo :platter puss"))
                   "platter puss")
     )
    (test-suite
     "ACTION"
     (test-false
      "rejects non-actions"
      (ACTION?
       (parse-irc-message "PRIVMSG #playroom :\u0001UNDERWEAR eats cornflakes\u0001")))
     (test-case
      "properly recognized and parsed"
      (let ((m (parse-irc-message "PRIVMSG #playroom :\u0001ACTION eats cornflakes\u0001")))
        (check-pred PRIVMSG? m)
        (check-pred ACTION? m)
        (check-equal? (PRIVMSG-text m) "eats cornflakes"))))
    )
   ))

(provide (all-defined-except message-command)
         (rename public-message-command message-command))
)
