(module score mzscheme
  (provide score-from-declarer-tricks)
  (define (score-from-declarer-tricks tricks auction)
#|
    
    ;; the auction will yield the level, the denomination, the risk, and
    ;; the vulnerability.
  
    ;; let's see.  How's this work?

    ;; http://ptaff.ca/bridge/duplicata/?lang=en_CA has a nice table of
    ;; scores, similar to those found on the backs of cards in your
    ;; bidding box

    ;; The ACBL's page on scoring:
    ;; http://www.acbl.org/learn/scoreDuplicate.html

    (if (negative? overtricks)
        (let ((undertricks (- overtricks)))
          (* 50
             (penalty-scale (vulnerable? auction)
                            (risk auction))
             undertricks)
          )
      (begin

        (cond
         ((minor-suit? (contract auction))
          (* tricks-taken 20))
         ((major-suit?  (contract auction))
          (* tricks-taken 30))
         (#t
          (+ 10 (* tricks-taken 30))))
        ))
|#
    0
          )
)
