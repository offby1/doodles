(module call mzscheme
  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))
  (require (lib "1.ss" "srfi"))
  (provide frobotzle)
  
  (define frobotzle
    (lambda ()
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
          (lambda (label level denom enabled?)
            (make-choice-button
             label
             (cdr (assq level bids))
             (cons level denom)
             enabled?)))
        
        (for-each (lambda (sym enabled?)
                    (make-choice-button
                     (format "~A" sym)
                     doubles-and-pass
                     sym
                     enabled?))
                  `(Double Redouble Pass)
                  `(#f #f #t)           ; BUGBUG -- get double and
                                        ; redouble right
                  )

        (for-each (lambda (level/pane-pair)
                    (for-each (lambda (denom)
                                (make-bid-button (format "~A ~A" (car level/pane-pair) denom) 
                                                 (car level/pane-pair)
                                                 denom
                                                 #t ; BUGBUG --
                                                   ; disable buttons
                                                   ; for illegal bids 
                                                   ))
                              `(clubs diamonds hearts spades notrump)))
                  bids)
        (send dialog show #t)
        choice)))

  (printf "~A~%" (frobotzle)))
