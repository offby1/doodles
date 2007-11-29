#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mred --no-init-file --mute-banner --version --require "$0"
|#
(module gui-constraints mzscheme
(require (lib "class.ss")
         (lib "mred.ss" "mred")
         (lib "pretty.ss"))


;; Trying to use MrEd to solve a problem that is easily solved in
;; Delphi 6, and presumably other sophisticated GUI builders: we have
;; a couple of widgets, and we'd like the state of one of those
;; widgets to control the state of the other.  Imagine a button and a
;; radio-box, where the button is enabled if and only if certain items
;; are selected in the radio box.  Now, we could certainly add code to
;; the callback for _the radio box_ that fiddles the enabled state of
;; the button, but imagine that we actually have lots of widgets, not
;; just two, and their interralationships are complex.  It seems to me
;; that it's easier to put the code that enables a widget right "next
;; to" that widget, and have that code refer to all the things that
;; affect it, than the other way around, namely, having each widget
;; "know" about those that it -affects-.

;; So my solution is to write a background thread that runs all the
;; "update me" processes.  This code here does just that, for a very
;; simple case.  Alas the background thread is burning up all the CPU;
;; I haven't yet figured out how to make it run only when it needs to
;; run (i.e., when some state in the GUI has changed).

;; * I think Kenny Tilton's "Cells"
;;   (http://common-lisp.net/project/cells/) are a sophisticated way
;;   to solve this problem (probably without threads)

;; * Java's "Swing" GUI thingy seems to have something called
;;   "Actions" that is like this:
;;   http://java.sun.com/docs/books/tutorial/uiswing/misc/action.html

;; * C-sharp has some documentation on "Event-based Asynchronous
;;   Pattern", which _might_ be relevant (but in typical M$ style,
;;   it's very hard to understand):
;;   http://msdn2.microsoft.com/en-us/library/wewwczdw.aspx

(with-handlers
    ([void void])
  (file-stream-buffer-mode (current-output-port) 'line)
  (file-stream-buffer-mode (current-error-port)  'line))

(define frame (new frame% (label "Constraint Testing")))

(define (usual-exception-handler e)
  (message-box
   "Uh oh"
   (cond
    ((exn? e)
     (exn-message e))
    (else
     (format "Unknown exception ~s" e))))

  (when (exn? e)
    (message-box
     "Oh, and"
     (format
      "~a"
      (let ((op (open-output-string)))
        (pretty-display
         (continuation-mark-set->context (exn-continuation-marks e))
         op)
        (get-output-string op))))))

(send frame create-status-line)

(define hpane (new horizontal-pane% (parent frame)))

(define open-button
  (new button% (parent frame) (label "Click me!")
       (callback (lambda (item event)
                   'golly))))

(define radio-box
  (new radio-box% (label "Pick one")
       (choices '("Disable that button there" "Another" "A Third"))
       (parent frame)))

(thread
 (lambda ()
   (let loop ()
     (sync (system-idle-evt))
     (send open-button enable
           (not (zero? (send radio-box get-selection))))
     (sleep 1/10)
     (loop))))

(let* ((mb (instantiate menu-bar% (frame)))
       (file-menu (instantiate menu% ("&File" mb)))
       (help-menu (instantiate menu% ("&Help" mb))))

  (instantiate
   menu-item%
   ("&Quit"
    file-menu
    (lambda (item event)
      (exit 0))))
  (instantiate
   menu-item%
   ("&About"
    help-menu
    (lambda (item event)
      (message-box
       "Exciting, huh?"
       (format "This is it."))))))

(send frame show #t)

)
