(require 'object->string)
(require 'random)
(require 'sort)
(require 'common-list-functions)
(require 'filter)

;; checks arglist ARGS to see if it contains the correct number and
;; types of arguments.  That information is in PREDS, which is a list
;; of predicates that describes the desired arguments.  For example,
;; if your arglist should be empty, make PREDS empty; if the arglist
;; should contain one number, make preds (number?); if the arglist
;; should contain a number and a list, make preds (number? list?).

;; Returns #t if the args are OK; otherwise calls ON-FAIL with a
;; string describing the problem.

(define (check-args args preds on-fail)
  (let ((result
         (if (not (= (length args)
                     (length preds)))
             (string-append "Incorrect number of arguments: "
                            (number->string (length args))
                            ".  Expected "
                            (number->string (length preds)))
           ;; Now see if the types match.
           (let loop ((preds preds)
                      (args args))

             (if (null? preds)
                 #t
               (if (not (apply (car preds)
                               (list (car args))))
                   (string-append "Incorrect argument type: "
                                  (object->string (car args))
                                  ".  Expected "
                                  (object->string (car preds)))
                 (loop (cdr preds)
                       (cdr args))))))))
    (if (not (eq? #t result))
	(on-fail result)
      #t)))

(define (make-suit integer-value)
  (let ((possible-values '(clubs diamonds hearts spades))
	(value '()))

    (define (initialized)
      (not (null? value)))

    (define (->integer)
      (position value possible-values))

    (if (or (< integer-value 0)
	    (>= integer-value 4))
	(error "Bad integer value for a suit:" integer-value))

    (set! value (list-ref possible-values integer-value))

    (lambda (op . args)
      (if (not (initialized))
	  (error "Uninitialized suit.")
	(case op
	  ((less-than?)
	   ;; return #f if and only if we outrank our argument, in terms
	   ;; of the bidding order
	   (check-args args (list procedure?) error)
	   (< (->integer)
	      ((car args) '->integer)))

	  ((->string)
	   (check-args args '() error)
	   ;; return a string that describes us
	   (object->string value))

	  ((->integer)
	   (check-args args '() error)
	   (->integer))

	  ((value)
	   (check-args args '() error)
	   value)

	  (else
	   (error "Unknown operation for suit:" op)))))))

(define (make-rank integer-value)
  (let ((possible-values '(two three four five six seven eight nine ten jack queen king ace))
	(value '()))

    (define (initialized)
      (not (null? value)))

    (define (->integer)
      (position value possible-values))

    (if (or (< integer-value 0)
	    (>= integer-value 13))
	(error "Bad integer value for a rank:" integer-value))

    (set! value (list-ref possible-values integer-value))

    (lambda (op . args)

      (if (not (initialized))
	  (error "Uninitialized rank.")
	(case op

	  ((less-than?)
	   ;; return #f if and only if we outrank our argument
	   (check-args args (list procedure?) error)
	   (< (->integer)
	      ((car args) '->integer)))

	  ((->string)
	   (check-args args '() error)
	   ;; return a string that describes us
	   (object->string value))

	  ((->integer)
	   (check-args args '() error)
	   (->integer))

	  ((value)
	   (check-args args '() error)
	   value)
	  (else
	   (error "Unknown operation for rank: " op)))))))

(define (make-card integer-value)

  (let ((suit '())
	(rank '())
	(possible-locations '(the-deck north east south west the-table))
	(location 'the-deck))

    (define (initialized)
      (not (or (null? suit)
	       (null? rank))))

    ;; Set the internal state to a RANK and SUIT determined from
    ;; the integer.
    (if (or (< integer-value
	       0)
	    (>= integer-value
		52))
	(error "Bad integer value for a card:" integer-value))

    (set! rank (make-rank (quotient integer-value
				    4)))
    (set! suit (make-suit (remainder integer-value
				     4)))
    (lambda (op . args)
      (if (not (initialized))
	  (error "Uninitialized card."))
      (case op
	((rank)
	 (check-args args '() error)
	 rank)

	((suit)
	 (check-args args '() error)
	 suit)

	((move!)
	 (check-args args (list (lambda (x)
				  (memq x
					possible-locations))) error)
	 (set! location (car args)))

	((->string)
	 (check-args args '() error)
	 ;; return a string that describes us
	 (string-append "The " (rank '->string)
			" of " (suit '->string)
			;;", in "
			;;(object->string location)
                        ))
	((location)
	 (check-args args '() error)
	 location)
	(else
	 (error "Unknown operation for card:" op))))))

(define the-deck

  (let ((result (make-vector 52)))

    (define (shuffle! vector)

      ;; Replace each entry X in the vector with a pair (X RAND).  X is
      ;; the original entry; RAND is a random number.

      (let loop ((entries-replaced 0))
	(if (< entries-replaced (vector-length vector))
	    (begin
              (vector-set! vector entries-replaced (cons (vector-ref vector entries-replaced)
                                                         (random most-positive-fixnum)))
              (loop (+ entries-replaced 1)))))

      ;; Sort the vector of pairs by the random number in each pair.
      (set! vector (sort! vector (lambda (x y)
				   (< (cdr x)
				      (cdr y)))))

      ;; Now strip off the random numbers.

      (let loop ((entries-stripped 0))
	(if (< entries-stripped (vector-length vector))
	    (begin
              (vector-set! vector entries-stripped (car (vector-ref vector entries-stripped)))
              (loop (+ entries-stripped 1))))))

    (let loop ((cards-initialized 0))
      (if (= cards-initialized (vector-length result))
	  result
	(begin
          (vector-set! result cards-initialized (make-card cards-initialized))
          (loop (+ cards-initialized 1)))))

    (shuffle! result)

    result))

;; Deal the cards.

(begin
  (display "Dealing cards...")
  (let loop ((cards-dealt 0))

    (define (appropriate-player n)
      (let ((valid-players '(east south north west)))
	(list-ref valid-players (remainder n (length valid-players)))))

    (if (< cards-dealt
	   (vector-length the-deck))
	(begin ((vector-ref the-deck cards-dealt)
		'move!
		(appropriate-player cards-dealt))
	       (loop (+ 1 cards-dealt)))))

  (display "done.\n"))

(define (card-list->string l separator)
  (accumulate (lambda (s1 s2)
		(string-append s1
			       (if (> (string-length s1)
				      1)
				   separator
				 "")
			       s2))
	      ""
	      (map (lambda (card)
		     (card '->string))
		   l)))

(define (held-by location)
  (filter (lambda (card)
	    (eq? location (card 'location)))
	  (vector->list the-deck)))

(define (high-card-point-count cards)
  (define (value card)
    (case ((card 'rank) 'value)
      ((ace)
       4)
      ((king)
       3)
      ((queen)
       2)
      ((jack)
       1)
      ((ten nine eight seven six five four three two)
       0)
      (else (error "Weird rank:" (card 'rank)))))

  (let loop ((total 0)
	     (cards cards))
    (if (null? cards)
	total
      (loop (+ total (value (car cards)))
	    (cdr cards)))))

(define (of-suit suit-symbol cards)
  (filter (lambda (card)
	    (eq? suit-symbol ((card 'suit) 'value)))
	  cards))

(define (suit-length-point-count cards)

  (define (point-value-for-suit-length sl)

    ;; Maybe we should add more if the length is in the trump suit.

    ;;(display (number->string (cdr sl)))
    ;;(display " ")
    ;;(display (object->string (car sl)))
    ;;(display "; ")

    (define (disp-num x)
      ;;(display (number->string x))
      ;;(display " points\n")
      x)
    (case (cdr sl)
      ((0 1 2 3 4)
       (disp-num 0))
      ((5 6)
       (disp-num (- (cdr sl)
	   4)))
      (else
       (disp-num (* 2 (- (cdr sl)
			 6))))))


  (define (get-suit-lengths cards)
    (map
     (lambda (suit-symbol)
       (cons suit-symbol (length (of-suit suit-symbol cards))))
     '(spades hearts diamonds clubs)))

  (let loop ((suit-lengths (get-suit-lengths cards))
	     (total 0))
    (if (null? suit-lengths)
	total
      (loop (cdr suit-lengths)
	    (+ total (point-value-for-suit-length (car
						   suit-lengths)))))))


;; Now display each holding.

(pretty-print (let ()
		(define (sort-hand cards)
		  (define (sort-by symbol cards)
		    (sort cards (lambda (x y)
				  ((x symbol) 'less-than? (y symbol)))))
		  (sort-by 'suit (sort-by 'rank cards)))
		(map
		 (lambda (location)
		   (let ((holding (held-by location)))

		     (string-append
		      "Holdings of "
		      (object->string location)
		      ":\n"
		      (card-list->string (sort-hand holding) "\n")
		      "\n"
		      "High card point value: "
		      (number->string (high-card-point-count holding))
		      "\n"

		      "Suit-length point value: "
		      (number->string (suit-length-point-count holding))

		      "\n")))

		 '(east west north south))))

(define (make-bid quantity suit)

  (let ((value (cons quantity suit)))

    (define (suit->number suit)
      (case suit
	((notrump)
	 4)

	((spades)
	 3)

	((hearts)
	 2)

	((diamonds)
	 1)

	((clubs)
	 0)

	(else
	 (error "Bad suit:" suit))))

    (define (bid->number bid)
      (+ (* 5 (car bid))
	 (suit->number (cdr bid))))

    (lambda (op args)
      (case op
	((less?)
	 (check-args args (list procedure?) error)
	 (< (bid->number value (bid->number (car args)))))
	(error
	 "Unimplemented bid op:" op)))))


(define bidding-history

  ;; You can ask if the auction is over.

  ;; If the auction is not over, you can enter a new bid into the
  ;; history.  Otherwise you get an error.

  ;; If the auction is over, you can ask for the contract.

  ;; At any time you can ask how many bids are in the history.

  ;; At any time you can ask for the nth bid.
  (let ((the-contract '())
	(the-auction
	 (let ((bids '()))

	   (lambda (op . args)

	     (case op
	       ((nth-bid)
		(check-args args (list number?) error)
		(list-ref bids (car args)))
	       ((is-legal add-bid)
		(error "Unimplemented auction op:" op))
	       (else
		(error "Unknown operation for auction:" op)))))))

    (lambda (op . args)
      (case op
	((nth-bid)
	 (check-args args (list number?) error)
	 ((the-auction 'nth-bid (car args))))

	((get-contract)
	 (check-args args '() error)
	 the-contract)

	((record-bid)
	 (check-args args (list procedure?) error)

	 (if (not (null? the-contract))
	     (error "Cannot record a bid when the auction is over.")

	   (let ((result (the-auction 'is-legal (car args))))
	     (if (eq? #t result)
		 (the-auction 'add-bid (car args))
	       (string-append "You can't add bid "
			      ((car args) '->string)
			      " to the auction because "
			      result)))))

	((auction-length)
	 (check-args args (list number?) error)
	 (length the-auction))

	((is-auction-over)
	 (check-args args '() error)
	 (not (null? the-contract)))

	(else
	 (error "Unknown operation for bid history:" op))))))