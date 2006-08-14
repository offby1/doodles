; Detect cycles in a link of conses.

; Sets up two pointers: the first points to the first cons; the second
; points to the first cons's cdr.  Got that?

; Then, while the two pointers aren't eq?, and neither points to nil,
;  set the first pointer to its cdr;
;  set the second pointer to its cddr, if the cdr is not nil;
;  otherwise set it to nil.

; if they're eq?, the link had a cycle.
; if the second one is nil, the link was a proper list.

; This algorithm is the answer to what is rumoured to be a nasty
; interview question at Microsoft...


(define (cyclic? ls)
  (if (null? ls)
      #f
    (let loop ((slow ls)
               (fast (cdr ls)))
      (cond ((eq? slow fast)
             #t)
            ((null? fast)
             #f)
            (else
             (loop (cdr slow)
                   (if (null? (cdr fast))
                       '()
                     (cddr fast))))))))



;; Some stuff to test it.

(define (make-cyclic! ls)

  ;; last-pair isn't officially part of R^4rs, but MIT C scheme and SCM
  ;; seem to have it.
  (define (last-pair ls)
    (list-tail ls (- (length ls) 1)))

  (set-cdr! (last-pair ls)
	    ls))

(let (( x '(a b c)))
  (make-cyclic! x)
  (cyclic? x))
