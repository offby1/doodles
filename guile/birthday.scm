;; How many people need you get together before it's likely that two
;; share a birthday?

;; Another way of saying that: how many people need you get together
;; before it's unlikely that none shares a birthday?

;; Here's how to solve the problem.  Imagine that you have a table
;; with some slots in it, and you also have some balls.  You toss each
;; ball at the table, and it winds up in a random slot.  After you've
;; thrown all the balls, some slots might be empty, some might have
;; exactly one ball, some might have more than one ball.  Now, notice
;; that each slot can represent a birthday, each ball can represent a
;; person, a ball in a particular slot can represent a person with a
;; particular birthday, and a slot with more than one ball can
;; represent more than one person sharing a birthday.  So the answer
;; to our original question is the same as the answer to "How many
;; balls do I need to throw at the table before it's likely that one
;; slot will contain at least two balls?"  And *that* question is
;; equivalent to "How many balls do I need to throw before it's
;; *unlikely* that *no slot* will contain more than one ball"?

;; (odds-all-distinct 23 365) ==> 0.492702765676014

(define (odds-all-distinct balls slots)

  (for-each
   (lambda (arg)
     (if (not (and (integer? arg)
                   (positive? arg)))
         (error "not a positive integer" arg)))
   (list balls slots))

  (if (= 1 balls)
      1
    (* (odds-all-distinct (- balls 1)
                          slots)
       (/ (- slots balls -1) slots)
       )))
