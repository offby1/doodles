(module play-history mzscheme
  (require
   "constants.ss"
   "exceptions.ss")
  (provide
   history-add
   history-length
   history-ref
   hand-complete?
   make-empty-history)
  
  (define-struct history (card-list) #f)
  
  (define (history-add h card)
    (when (member card (history-card-list h))
      (raise-bridge-error 'history-add "unique" card))
    (make-history (append (history-card-list h) (list card))))
  
  (define (history-length h)
    (length (history-card-list h)))
  
  (define (history-ref h K)
    (list-ref (history-card-list h) K))
  
  (define (make-empty-history)
    (make-history '()))

  (define (hand-complete? h)
    (= (length (history-card-list h))
       *deck-size*))
  )
