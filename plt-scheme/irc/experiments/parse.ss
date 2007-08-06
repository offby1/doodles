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
         (only (planet "port.ss" ("schematics" "port.plt" 1 0))
               port->string-list)
         (only (lib "1.ss" "srfi")
               second
               third
               fifth
               fourth)
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

(define (p val)
  (write val)
  (newline)
  val)

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
      (printf "sans-command: ~s~%" sans-command)
      (let* ((m (p (regexp-match #rx"((.*?) )?(:(.*))?$" sans-command)))
             (middle-params (string-tokenize
                             (or (second m) "")
                             (char-set-complement (char-set #\space))))
             (trailing-parameter (if (fifth m) (list (fifth m))
                                   '())))
        (make-message prefix command (append middle-params  trailing-parameter))))))

(trace parse-irc-message)

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

   (test-parse ":foo bar baz :"                         "foo" "bar" '("baz" ""))
   (test-parse ":foo bar baz :params go here"          "foo" "bar" '("baz" "params go here"))
   (test-parse ":localhost. NOTICE you :all suck"
               "localhost."
               "NOTICE"
               '("you" "all suck"))
   (test-parse ":localhost. PRIVMSG :e1f: you all suck"
               "localhost."
               "PRIVMSG"
               '("e1f: you all suck"))
   (test-parse
    ":ChanServ!ChanServ@services. MODE #cinema +tc "
    "ChanServ!ChanServ@services."
    "MODE"
    '("#cinema" "+tc"))
   ;;     (test-parse "foo "                                  )

   ;;     (test-equal?
   ;;      "prefix"
   ;;      (message-prefix (parse-irc-message ":zip zap zop :snot"))
   ;;      "zip")
   ;;     (test-false
   ;;      "missing prefix"
   ;;      (message-prefix (parse-irc-message "NOTICE All Apple fanbois will be taken out back")))
   ;;     (test-equal?
   ;;      "command"
   ;;      (message-command (parse-irc-message "NOTICE All Apple fanbois will be taken out back"))
   ;;      'NOTICE)
   ;;     (test-equal?
   ;;      "real params (not ust trailing)"
   ;;      (message-command (parse-irc-message "COMMAND foo bar baz"))
   ;;      (list "foo" "bar" "baz"))
   ;;     (test-equal?
   ;;      "trailing params (not ust trailing)"
   ;;      (message-command (parse-irc-message "COMMAND poo poo :platter puss"))
   ;;      (list "poo" "poo" "platter puss"))


   ))

(provide (all-defined))
)
