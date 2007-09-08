#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui repl-tests 'verbose))"
|#
(module repl mzscheme
(require (only (lib "1.ss" "srfi") delete)
         (lib "kw.ss")
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (prefix bot: "bot.ss")
         "session.ss")

(define/kw (make-mru #:optional [initial '()])
  initial)

(define (mru-add mru item)
  (cons item (delete item mru)))

(define (mru-remove mru item)
  (delete item mru))

(define mru-member member)

;; this is actually a list of channels that we're currently joined
;; to.  And it's a terrible idea, since the session object has a
;; similar list, and I can't think of a good way to keep the two lists
;; in sync.
(define *joined-channels* #f)

(define/kw (pm text #:key [destination (car *joined-channels*)])
  (bot:pm bot:*sess* destination text))

(define (join channel)
  (bot:out bot:*sess* "JOIN ~a~%" channel)
  (select channel))

(define (select channel)
  (when (not (member channel *joined-channels*))
    (error 'select "channel ~s is not in ~s" channel *joined-channels*))
  (set! *joined-channels* (mru-add *joined-channels* channel)))

(define/kw (me text #:key [channel (car *joined-channels*)])
  (bot:pm bot:*sess* channel (format "\u0001ACTION ~a\u0001~%" text)))

(define/kw (part #:key [channel (car *joined-channels*)])
  (bot:out bot:*sess* "PART ~a~%" channel)
  (set! *joined-channels* (mru-remove *joined-channels* channel)))

(define (run-repl)
  (set! *joined-channels* (make-mru (irc-session-joined-channels bot:*sess*)))
  (read-eval-print-loop))


(provide run-repl)

)
