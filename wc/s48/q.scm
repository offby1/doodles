;;; Stolen from SICP, and scheme48-ified

(define-record-type :queue
  (make-queue-record front-ptr rear-ptr)
  queue?
  (front-ptr queue-front-ptr set-queue-front-ptr!)
  (rear-ptr  queue-rear-ptr  set-queue-rear-ptr!))

(define (empty-queue? q) (null? (queue-front-ptr q)))

(define (front-queue q)
  (if (empty-queue? q)
      (error "FRONT called with an empty queue" q)
    (car (queue-front-ptr q))))

(define (insert-queue! q item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? q)
           (set-queue-front-ptr! q new-pair)
           (set-queue-rear-ptr! q new-pair)
           q)
          (else
           (set-cdr! (queue-rear-ptr q) new-pair)
           (set-queue-rear-ptr! q new-pair)
           q))))

(define (delete-queue! q)
  (cond ((empty-queue? q)
         (error "DELETE! called with an empty queue" q))
        (else
         (set-queue-front-ptr! q (cdr (queue-front-ptr q)))
         q)))

(define (make-queue seq)
  (let ((rv (make-queue-record '() '())))
    (for-each (lambda (item) (insert-queue! rv item))
              seq)
    rv))
