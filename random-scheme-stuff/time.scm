;; This expression will cause Scheme to barf unless you've prepared it for macros by doing the following:
;;   (require 'macro)
;;   (require 'repl)
;;   (repl:top-level macro:eval)

;; Note that get-internal-run-time seems to be unique to SCM -- MIT C
;; scheme, for example, doesn't have it.  Bummer.
;;
;; Gambit, on the other hand, already has a `time' macro.

(define-syntax time
  (syntax-rules ()
                ((time expr)
                 (let* ((start (get-internal-real-time))
                        (result expr)
                        (stop (get-internal-real-time)))
                   (display "time `")
                   (display 'expr)
                   (display "': ")
                   (display (- stop start))
                   (newline)
                   result))))
