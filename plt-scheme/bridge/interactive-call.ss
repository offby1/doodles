(module interactive-call mzscheme
  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))
  (require (all-except (lib "1.ss" "srfi") reverse! member map
                       for-each assoc append!))
  (require "call.ss")
  (require "misc.ss")
  (require "auction.ss")
  (provide make-bbox-window
           reset-buttons-for-new-auction
           interactively-get-call)
  
  (define *choice* #f)
  (define *sem* (make-semaphore))

  (define *bidding-box-window* #f)
  (define *column* #f)
  (define doubles-and-pass #f)
  (define *level/pane-alist* #f)

  (define (make-bbox-window parent)
    (when *bidding-box-window*
      (error "I expected *bidding-box-window* to be #f"))
    (set! *bidding-box-window* 
          (instantiate frame% ()
            (parent parent)
            (enabled #t)
            (label "If you can see this, something is wrong.")))
    (set! *column* (instantiate vertical-pane% ()
                     (parent *bidding-box-window*)))

    (set! doubles-and-pass (instantiate horizontal-pane% ()
                             (parent *column*)))

    (set! *level/pane-alist* (map (lambda (level)
                                    (cons level (instantiate horizontal-pane% ()
                                                  (parent *column*))))
                                  (reverse (iota 7 1))))

    (for-each (lambda (sym enabled?)
                (make-choice-button
                 (call->string sym)
                 doubles-and-pass
                 sym
                 enabled?))
              `(double redouble pass)
              `(#f #f #t)               ; BUGBUG -- get double and
                                        ; redouble right
              )

    (for-each (lambda (level/pane-pair)
                (for-each (lambda (denom)
                            (let ((b (make-bid (car level/pane-pair) denom)))
                              (make-bid-button b)))
                          *denominations*))
              *level/pane-alist*)

    (unless (= 38 (length *call/button-alist*))
      (error "I was expecting exactly 38 buttons in *call/button-alist*, but instead it's "
             (length *call/button-alist*)))
    )

  ;; all 38 buttons (38 = 7 levels * 5 denominations plus 3 {pass,
  ;; double, redouble})
  (define *call/button-alist* '())

  (define (reset-buttons-for-new-auction)
    (for-each (lambda (call/button-pair)
                (send (cdr call/button-pair)
                      enable #t))
              *call/button-alist*)
    (send (cdr (assq 'double   *call/button-alist*)) enable #f)
    (send (cdr (assq 'redouble *call/button-alist*)) enable #f)
    )
  
  (define interactively-get-call #f)

  (define (make-choice-button label parent value enabled?)
    (let ((button (instantiate button% () (label label)
                               (parent parent)
                               (enabled enabled?)
                               (callback (lambda (button control-event-object)
                                           (set! *choice* value)
                                           (send *bidding-box-window* show #f)
                                           (semaphore-post *sem*))))))
      (set! *call/button-alist* (cons (cons value button)
                                      *call/button-alist*))
      button
      ))

  (define make-bid-button
    (lambda (b)
      (make-choice-button
       (call->string b)
       (cdr (assq (bid-level b) *level/pane-alist*))
       b
       #t)))

  (set! interactively-get-call
        (lambda (the-auction frame-title)
          (let ((highest-illegal-bid (last-bid the-auction)))
            ;; disable some more buttons.
            ;; BUGBUG: do the double and redouble too
            (when highest-illegal-bid
              (for-each (lambda (call/button-pair)
                          (when (and (bid? (car call/button-pair))
                                     (not (bid> (car call/button-pair) highest-illegal-bid)))
                            (send (cdr call/button-pair)
                                  enable #f)))
                        *call/button-alist*))
            (send *bidding-box-window* set-label frame-title)
            (send *bidding-box-window* show #t)
            (yield *sem*)
            *choice*)))
  
  (when #f
    ;; a very short auction :-\
    (make-bbox-window #f)
    (let ((c1 (interactively-get-call #f "Testing .. testing .. is this thing on?")))
      (let ((c2 (interactively-get-call (and (bid? c1) c1) "thump thump")))
        (printf "~A~%" (call->string
                        c2)))
      )))
