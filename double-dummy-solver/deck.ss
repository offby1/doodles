#! /bin/sh
#| Hey Emacs, this is -*-mode: scheme; coding:utf-8 -*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module deck mzscheme
(require "card.ss"
         "dds.ss"
         "history.ss"
         "zprintf.ss"
         (only "trick.ss" *seats*)
         (prefix ha: "hand.ss")
         (lib "pretty.ss")
         (lib "cmdline.ss")


         (only (lib "list.ss") sort)
         (only (lib "1.ss" "srfi") iota take circular-list ))
(define max-lookahead 0)
(random-seed 0)


(define *deck*
  (let loop ((suits *suits*)
             (result '()))
    (if (null? suits)
        (sort result (lambda (a b)
                       (< (card-rank a)
                          (card-rank b))))
      (loop (cdr suits)
            (append
             (let loop ((ranks (iota *num-ranks* 2))
                        (result '()))
               (if (null? ranks)
                   result
                 (loop (cdr ranks)
                       (cons (make-card (car suits)
                                        (car ranks))
                             result))))
             result)))))

(define (fisher-yates-shuffle! v)
  (define (swap! i1 i2)
    (let ((tmp (vector-ref v i1)))
      (vector-set! v i1 (vector-ref v i2))
      (vector-set! v i2 tmp)))
  (let ((l (vector-length v)))
    (do ((top-index (sub1 l) (sub1 top-index)))
        ((zero? top-index) v)
      (let ((bottom-index (random top-index)))
        (swap! bottom-index top-index)))))


(define *num-hands* (make-parameter 1))
(command-line
 "dds"
 (current-command-line-arguments)
 (once-each
  (("-f" "--fancy-suits") "Display little suit symbols instead of c, d, h, s"
   (*fancy-suits* #f))
  (("-q" "--quiet") "Suppress all diagnostics"
   (*shaddap* #t))
  (("-l" "--lookahead") ml
   "Maximum number of tricks to look ahead when predicting"
   (set! max-lookahead (string->number ml)))
  (("-n" "--num-hands") nh
   "Number of hands to deal"
   (*num-hands* (string->number nh)))
  ))

(time
 (for-each
  (lambda (hand-number)
    (define hands (map (lambda (s) (ha:make-hand '() s)) *seats*))

    ;; deal 'em out
    (let loop ((d (vector->list (fisher-yates-shuffle! (list->vector *deck*))))
               (hs (apply circular-list hands)))
      (unless (null? d)
        (let ((victim (car hs)))
          (ha:add-card! victim (car d)))

        (loop (cdr d)
              (cdr hs))))

    ;; sort the hands.  This is actually important, since
    ;; group-into-adjacent-runs will be more likely to return exactly 1
    ;; group, and hence things will go faster.
    (for-each ha:sort!  hands)

    (display #\page) (newline)
    (printf "~a~%" (make-string 60 #\=))
    (printf "Hand ~a~%" hand-number)
    (printf "~a~%" (make-string 60 #\=))

    (for-each (lambda (h)
                (display h)
                (newline))  hands)
    (newline)

    (play-loop
     (make-history (car *seats*))
     hands
     max-lookahead

     ;; always returns false -- thus we'll stop only when the hands
     ;; have been emptied.
     (lambda args #f)

     (lambda (hi hands)
       (printf "~a -> ~a~%" hi (compute-score hi))))
    (printf "~%~%~%"))

  (iota (*num-hands*)))))


