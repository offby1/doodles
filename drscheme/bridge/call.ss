(module call mzscheme
  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))
  (require (lib "1.ss" "srfi"))
  (provide make-call call->string)
  
  (define *denominations* `(clubs diamonds hearts spades notrump))
  (define-struct bid (level denomination))
  (define (bid->int b)
    (define (index item seq ) (- (length seq) (length (member item seq))))
    (+ (* (sub1 (bid-level b)) (length *denominations*))
       (index (bid-denomination b) *denominations*)))
  (define (bid->string b)
    (format "~A ~A" (bid-level b)
            (bid-denomination b)))
  (define (call->string c)
    (if (bid? c)
        (bid->string c)
      (format "~A" c)))
  
  (define make-call
    (lambda (highest-illegal-bid)
      (let* ((dialog (instantiate dialog% () (label "Make a call.")))
             (column (instantiate vertical-pane% ()
                       (parent dialog)))

             ;; alist, each elt looks like (3 . <horizontal-pane%>)
             (bids (map (lambda (level)
                          (cons level (instantiate horizontal-pane% ()
                                        (parent column))))
                        (reverse (iota 7 1))))
             (doubles-and-pass (instantiate horizontal-pane% ()
                                 (parent column)))
             (choice #f))

        (define (make-choice-button label parent value enabled?)
          (instantiate button% () (label label)
                       (parent parent)
                       (enabled enabled?)
                       (callback (lambda (button control-event-object)
                                   (set! choice value)
                                   (send dialog show #f)))))
        (define make-bid-button
          (lambda (b enabled?)
            (make-choice-button
             (bid->string b)
             (cdr (assq (bid-level b) bids))
             b
             enabled?)))
        
        (for-each (lambda (sym enabled?)
                    (make-choice-button
                     (call->string sym)
                     doubles-and-pass
                     sym
                     enabled?))
                  `(Double Redouble Pass)
                  `(#f #f #t)           ; BUGBUG -- get double and
                                        ; redouble right
                  )

        (for-each (lambda (level/pane-pair)
                    (for-each (lambda (denom)
                                (let ((b (make-bid (car level/pane-pair) denom)))
                                  (make-bid-button b
                                                   (or (not highest-illegal-bid)
                                                       (> (bid->int b)
                                                          (bid->int highest-illegal-bid)))
                                                   )))
                              *denominations*))
                  bids)
        (send dialog show #t)
        choice)))

  (when #f
    ;; a very short auction :-\
    (let ((c1 (make-call #f)))
      (let ((c2 (make-call (and (bid? c1) c1))))
        (printf "~A~%" (call->string
                        c2)))
      )))
