#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require (lib "list.ss" "srfi" "1"))
(require (lib "trace.ss"))

(define *the-channel* (make-channel))

(define consumer
  (lambda ()
    (let loop ((ca (channel-get *the-channel*)))
      (if ca
          (begin
            (printf "Consumer got a completed auction: ~s~n" ca)
            (loop (channel-get *the-channel*)))
        (printf "Consumer got #f, so that means it's time to quit.~n"))
      )))

;; simple function that acts vaguely like
;; "some-auctions-with-given-prefix"
(define makes-big-lists
  (let ((passes 10))
    (lambda (seq max exit)
      (define (allowable-successors seq max)
        (let ((l (last seq)))
          (iota (- max l) (+ 1 l))))
      (for-each (lambda (n)
                  (let ((extended (append seq (list n))))
                    (if (= n max)
                        (begin
                          (printf "Producer: passes is ~a; sending ... " passes)
                          (channel-put *the-channel* extended)
                          (set! passes (- passes 1))
                          (printf "passes is now ~a~n" passes)
                          (when (zero? passes)
                            (channel-put *the-channel* #f)
                            (printf "Producer exiting~n")
                            (exit)))
                      (makes-big-lists extended max exit))))
                (allowable-successors seq max))))
  )
;(trace makes-big-lists)
(define consumer-thread-id (thread consumer))
(call/cc
 (lambda (exit)
   (makes-big-lists '(0) 10 exit)))
