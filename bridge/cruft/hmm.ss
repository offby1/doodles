(require (lib "list.ss" "srfi" "1"))
;; simple function that acts vaguely like
;; "some-auctions-with-given-prefix"
(define (makes-big-lists seq max)
  (when (any (lambda (x)
               (> x max))
             seq)
    (raise-type-error 'makes-big-lists (format "list of integers <= ~a" max) seq))
  (append-map (lambda (n)
                (let ((extended (append seq (list n))))
                  (if (= n max)
                      (list extended)
                    (makes-big-lists extended max))))
              (allowable-successors seq max)))

(define (allowable-successors seq max)
  (let ((l (last seq)))
    (iota (- max l) (+ 1 l))))

(define (mbl2 seq max)
  (when (any (lambda (x)
               (> x max))
             seq)
    (raise-type-error 'makes-big-lists (format "list of integers <= ~a" max) seq))
  (lazy-map (lambda (n)
              (let ((extended (append seq (list n))))
                (if (= n max)
                    (list extended)
                  (makes-big-lists extended max))))
            (allowable-successors seq max)))

;; => () | ( value . promise )
(define (lazy-map proc seq)
  (cond 
   ((null? seq)
    '())
   (else
    (cons (proc (car seq))
          (delay (lazy-map proc (cdr seq)))))))

(define (force-em-all p)
  (cond
   ((promise? p)
    (force-em-all (force p)))
   ((pair? p)
    (cons (force-em-all (car p))
          (force-em-all (cdr p))))
   (else
    p)
   ))

(define (forcing-for-each proc seq)
  (let loop ((seq seq))
    (unless (null? seq)
      (proc (car seq))
      (forcing-for-each proc (force (cdr seq))))))

(define-syntax enque-unique!
  (syntax-rules ()
    ((_ node q)
     (when (not (member node q))
       (set! q (append q (list node)))))))

(define-syntax pop!
  (syntax-rules ()
    ((_ q)
     (begin
       (when (null? q)
         (error "Can't pop no empty queue."))
       (let ((rv (car q)))
         (set! q (cdr q))
         rv)))))

(define node-neighbors
  (let ((max 10))
    (lambda (x)
      (cons (- x)
            (iota (- max x)
                  (+ 1 x))))))

(define (bfs proc node queue)
  (proc node)
  (for-each (lambda (n)
              (enque-unique! n queue))
            (node-neighbors node))
  (let ((new (pop! queue)))
    (bfs proc new queue)))
