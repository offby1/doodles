#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mred -qu "$0" ${1+"$@"}
|#

(module test mzscheme
(require (lib "cards.ss" "games" "cards")
         (lib "classes.ss" "games" "cards")
         (lib "class.ss"))
(define *t* (make-object table% "Golly" 5 5))
(define *d* (make-deck))
(define *r* (make-region 0 0 1000 1000 "A Region" #f))

(send *t* add-cards-to-region *d* *r*)
(send
 *t*
 set-double-click-action
 (lambda (card)
   (printf "Double-clicked ~s~%" card)))

(send *t* show #t)
)