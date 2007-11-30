#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mred --no-init-file --mute-banner --version --require "$0"
|#
(module gui-constraints mzscheme
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
;; interrelationships are complex.  It seems to me that it's easier to
;; put the code that enables a widget right "next to" that widget, and
;; have that code refer to all the things that affect it, than the
;; other way around, namely, having each widget "know" about those
;; that it -affects-.

;; So my solution is to write a background thread that runs all the
;; "update me" processes.  This code here does just that, for a very
;; simple case.

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

(define frame (new frame% (label "Constraint Testing")))

(define button
  (new button% (parent frame) (label "Click me!")
       (enabled #f)
       (callback (lambda (item event)
                   (message-box "OK" "Now what?")))))

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

(define radio-box
  (new (blabbermouth-control% radio-box%) (label "Pick one")
       (choices '("Disable that button there" "Let it be enabled"))
       (parent frame)))

(define checkbox (new (blabbermouth-control% check-box%)
                               (label  "Invert the sense of the above")
                               (parent frame)))

(define whop-button
  (lambda ()
    (send button enable
          (even?
           (length
            (filter
             (lambda (x) x)
             (list
              (send checkbox get-value)
              (zero? (send radio-box get-selection)))))))))

(for-each (lambda (widget) (send widget register-listener whop-button))
          (list radio-box checkbox))

(send frame show #t)

)
