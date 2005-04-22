#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require (lib "list.ss" "srfi" "1"))
(require (lib "trace.ss"))

(define *the-channel* (make-channel))
(define *producer-keep-going* #t)

(define consumer
  (lambda ()
    (let loop ((ca (channel-get *the-channel*))
               (passes 10))
      (if (zero? passes)
          (begin
            (printf "Consumer used up its passes, so that means it's time to quit.~n")
            (set! *producer-keep-going* #f))
        (begin
          (printf "Consumer got a completed auction: ~s~n" ca)
          (loop (channel-get *the-channel*)
                (- passes 1))))
      
      )))

;; simple function that acts vaguely like
;; "some-auctions-with-given-prefix"
(define makes-big-lists
  (lambda (seq max)
    (define (allowable-successors seq max)
      (let ((l (last seq)))
        (iota (- max l) (+ 1 l))))
    (for-each (lambda (n)
                (when  *producer-keep-going*
                  (let ((extended (append seq (list n))))
                    (if (= n max)
                        (begin
                          (printf "Producer: sending ~s~n" extended)
                          (channel-put *the-channel* extended))
                      (makes-big-lists extended max)))))
              (allowable-successors seq max))
    )
  )
;(trace makes-big-lists)
(define consumer-thread-id (thread consumer))
(makes-big-lists '(0) 10)
