#! /bin/sh
#| Hey Emacs, this is -*-mode: scheme; coding:utf-8 -*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module test-predictions mzscheme
(require "card.ss"
         "dds.ss"
         "deck.ss"
         "fys.ss"
         "history.ss"
         "zprintf.ss"
         (only "trick.ss" *seats* *trump-suit*)
         (prefix ha: "hand.ss")
         (lib "pretty.ss")
         (lib "cmdline.ss")


         (only (lib "list.ss") sort)
         (only (lib "1.ss" "srfi")
               circular-list
               drop-right
               fold
               iota
               last-pair
               take
               ))
(display "$Id$" (current-error-port))
(newline (current-error-port))
(define max-lookahead 0)
(random-seed 0)


(define *num-hands* (make-parameter 1))
(command-line
 "dds"
 (current-command-line-arguments)
 (once-each

  ;;Seeing octal escapes in Emacs?  Try C-x RET c utf-8 RET M-x make
  ;;And use Lucida Console, if your terminal is running on Windows; on
  ;;*nix, '10x20' from Emacs' font menu also works
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

(printf "Howdy.  'test-predictions.ss' here.~%")
(time
 (for-each
  (lambda (hand-number)

    (define hands (deal (vector->list (fisher-yates-shuffle! (list->vector *deck*)))
                        (map (lambda (s) (ha:make-hand '() s)) *seats*)))

    (for-each
     (lambda (trump-suit)
       (parameterize ((*trump-suit* trump-suit))

         ;; sort the hands.  This is actually important, since
         ;; group-into-adjacent-runs will be more likely to return exactly 1
         ;; group, and hence things will go faster.
         (for-each ha:sort!  hands)

         (when (not (*trump-suit*))
           (display #\page) (newline)
           (printf "~a~%" (make-string 60 #\=))
           (printf "Hand ~a~%" hand-number)
           (printf "~a~%" (make-string 60 #\=))

           (for-each (lambda (h)
                       (display h)
                       (newline))  hands)
           (newline))

         (printf "Trump suit: ~a => "
                 (or (*trump-suit*)
                     "notrump"))
         (play-loop
          (make-history (car *seats*))
          hands
          choose-card
          max-lookahead
          ;; always returns false -- thus we'll stop only when the hands
          ;; have been emptied.
          (lambda args #f)

          (lambda (hi hands)
            (printf "~a~%" hi)))
         ))
     (cons #f *suits*)
     ))
  (iota (*num-hands*)))))
