(module call mzscheme
  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))

  (provide make-call)

  (define (multiply seq1 seq2)
    (define (distribute atom lst)
      (if (null? lst)
          lst
        (map (lambda (thing) (cons atom thing))
             
             lst)
        ))
    (if (null? seq1)
        seq2
      (apply append (map (lambda (atom) (distribute atom seq2))
                         seq1))
      ))
  
  (define my-get-choices
    (lambda (options)
      (let ((dialog (instantiate dialog% () (label "Make a call.")))
            (choice #f))
        (define make-button
          (lambda (label)
            (instantiate button% () (label label)
                         (parent dialog)
                         (callback (lambda (button control-event-object)
                                     (set! choice label)
                                     (send (send button get-parent) show #f))))))
        (for-each (lambda (l)
                    (make-button l))
                  options)
        (send dialog show #t)
        choice)))
  
  (define (make-call player auction)
    
    (let* ((levels `(1 2 3 4 5 6 7))
           (denominations `(clubs diamonds hearts spades notrump))
           (bids (cons "redouble"
                       (cons "double"
                             (cons "pass"
                                   (map (lambda (p)
                                          (format "~A ~A" (car p)
                                                  (cdr p)))
                      
                                        (multiply levels denominations)))))))
      (my-get-choices bids)))
  
  (printf "~A~%"  (make-call 'foo 'bar))
  (printf "~A~%" (my-get-choices (list "one" "two" "three"))))
