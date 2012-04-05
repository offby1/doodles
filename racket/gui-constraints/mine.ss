#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mred --no-init-file --mute-banner --version --require "$0"
|#
(module mine mzscheme
(require (lib "class.ss")
         (lib "mred.ss" "mred")
         (only (lib "1.ss" "srfi")
               filter))

;; Trying to use MrEd to solve a problem that is easily solved in
;; Delphi 6, and presumably other sophisticated GUI builders: we have
;; a couple of widgets, and we'd like the state of each of those
;; widgets to be determined by the states of some of the others.
;; Imagine a button and a radio-box, where the button is enabled if
;; and only if certain items are selected in the radio box.  Now, we
;; could certainly add code to the callback for _the radio box_ that
;; fiddles the enabled state of the button, but imagine that we
;; actually have lots of widgets, not just two, and their
;; interrelationships are complex.  In that case, it seems to me that
;; it's easier to put the code that enables a widget right "next to"
;; that widget, and have that code refer to all the things that affect
;; it, than the other way around, namely, having each widget "know"
;; about those that it -affects-.

;; So my solution is to have the "upstream" widgets use a special
;; class that notifies "listeners".  Then the "downstream" widgets can
;; "register" their interest in the upstream's state changes.  This
;; code does just that, for a very simple case.

;; * I think Kenny Tilton's "Cells"
;;   (http://common-lisp.net/project/cells/) are a sophisticated way
;;   to solve this problem (probably without threads)

;; * Java's "Swing" GUI thingy has something called "Actions" that
;;   might solve the same problem:
;;   http://java.sun.com/docs/books/tutorial/uiswing/misc/action.html;
;;   "hotblack23" on #scheme suggests that
;;   java.beans.PropertyChangeListener is the relevant class

;; * C-sharp has some documentation on "Event-based Asynchronous
;;   Pattern", which _might_ be relevant (but in typical M$ style,
;;   it's very hard to understand):
;;   http://msdn2.microsoft.com/en-us/library/wewwczdw.aspx

;; * Peter Ivanyi's "MrEd Designer"
;;   (http://www.hexahedron.hu/personal/peteri/mreddesigner/index.html)
;;   solves exactly this problem in its "graph editor"

(define frame (new frame% (label "Constraint Testing")))

(send frame create-status-line)
(send frame set-status-text "Nothin' yet.")

(define blabbermouth-control%
  (mixin (control<%>) (control<%>)
         (init-field (callback void))
         (field (listeners '()))

         (define/public (register-listener thunk)
           (set! listeners (cons thunk listeners)))

         (super-instantiate
          ()
          (callback
           (lambda (item event)
             (for-each (lambda (cb)
                         (queue-callback cb #f))
                       listeners)
             (callback item event))))))

(define checkbox (new (blabbermouth-control% check-box%)
                      (label  "Invert the sense of the above")
                      (parent frame)))

(define radio-box
  (new (blabbermouth-control% radio-box%) (label "Pick one")
       (choices '("Disable that button there" "Let it be enabled"))
       (parent frame)))

(define (desired-button-state)
  (even?
   (length
    (filter
     (lambda (x) x)
     (list
      (send checkbox get-value)
      (zero? (send radio-box get-selection)))))))

(define button
  (new (blabbermouth-control% button%) (parent frame) (label "Click me!")
       (enabled (desired-button-state))))

(for-each
 (lambda (widget)
   (send
    widget
    register-listener
    (lambda ()
      (send button enable (desired-button-state)))))
 (list radio-box checkbox))

(send button register-listener
      (let ((count 0))
        (lambda ()
          (set! count (add1 count))
          (let ((label (format "~a times, now, the damned button has changed state"
                               count)))

            (send checkbox command (new control-event% (event-type 'check-box)))
            (send checkbox set-value (not (send checkbox get-value)))

            (send frame set-status-text label)))))

(send frame change-children reverse)

(send frame show #t)

)
