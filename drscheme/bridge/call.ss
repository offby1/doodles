(module call mzscheme
  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))
  (require (lib "1.ss" "srfi"))
  (provide make-call)
  
  (define *denominations* `(clubs diamonds hearts spades notrump))
  (define-struct bid (level denomination))
  (define (bid-value b)
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
    (lambda (lowest-legal-bid)
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
                                                   (or (not lowest-legal-bid)
                                                       (>= (bid-value b)
                                                           (bid-value lowest-legal-bid)))
                                                   )))
                              *denominations*))
                  bids)
        (send dialog show #t)
        choice)))

  (printf "~A~%" (call->string
                  (make-call (make-bid 3 'notrump)))))
