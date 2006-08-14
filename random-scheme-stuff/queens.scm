(require 'filter)

  ;;;;;;;;;;;;;;;;;;;; square

(define square:row car)
(define square:column cdr)
(define square:make cons)

  ;;;;;;;;;;;;;;;;;;;; square-array

(define (square-array:make size value)
  (let ((result (make-vector size)))
    (let loop ((to-do size))
      (if (= to-do 0)
	  result
	(begin
	  (vector-set! result (- to-do 1) (make-vector size value))
	  (loop (- to-do 1)))))))

(define (square-array:copy array)
  (let ((result (square-array:make (square-array:size array) #f)))
    (let loop ((rows-copied 0))
      (define (copy-vector v)
	(list->vector (vector->list v)))
      (if (= rows-copied (square-array:size array))
	  result
	(begin
	  (vector-set! result rows-copied (copy-vector (vector-ref
							array
							rows-copied)))
	  (loop (+ 1 rows-copied)))))))

(define (square-array:->string array)

  (define (vector->string v)
    (define (object->string o)
      (make-string 1 o))
    (let loop ((cells-processed 0)
	       (result ""))
      (if (< cells-processed (vector-length v))
	  (loop (+ 1 cells-processed)
		(string-append result (object->string (vector-ref v cells-processed))))
	result)))

  (let loop ((rows-processed 0)
	     (result ""))
    (if (< rows-processed (square-array:size array))
	(loop (+ 1 rows-processed)
	      (string-append result
			     (vector->string (vector-ref array rows-processed))
			     "\n"))
      result)))

(define (square-array:size array)
  (vector-length array))

(define (square-array:get row column array)
  (vector-ref (vector-ref array row) column))

(define (square-array:set! row column array value)
  (let ((temp-row (vector-ref array row)))
    (vector-set! temp-row column value)
    (vector-set! array row temp-row)))

  ;;;;;;;;;;;;;;;;;;;; board

(define initial-board-size 4)

(define (board:make size)
  (square-array:make size #\_))

(define board:->string square-array:->string)
(define board:copy square-array:copy)
(define board:size square-array:size)

(define (adjoin board-list1 board-list2)
  (define (present? board board-list)
    (if (null? board-list)
	#f
      (if (equal? board (car board-list))
	  #t
	(present? board (cdr board-list)))))
  (cond ((null? board-list1)
	 board-list2)
	((null? board-list2)
	 board-list1)
	(#t
	 (if (present? (car board-list1) board-list2)
	     (adjoin (cdr board-list1)
		     board-list2)
	   (cons  (car board-list1)
		  (adjoin (cdr board-list1)
			  board-list2))))))

(define (board:occupy! square board)

  (define (board:contains? square board)
    (and
     (< (square:row    square) (board:size board))
     (< (square:column square) (board:size board))
     (>= (square:row    square) 0)
     (>= (square:column square) 0)))

  (define (queens-attacked-squares-list square board)

    (define (flatmap proc seq)
      (define (accumulate proc initial list)
	(if (null? list)
	    initial
	  (proc initial (accumulate proc (car list) (cdr list)))))
      (accumulate append '() (map proc seq)))

    (define (enumerate-interval start end)
      (if (> start end)
	  '()
	(cons start (enumerate-interval (+ start 1) end))))

    (define (list-of-distant-squares distance square)

      (map (lambda (offset-pair)
	     (square:make (+ (square:row    square) (car offset-pair))
			  (+ (square:column square) (cdr offset-pair))))
	   (let ((rows (square:row square))
		 (cols (square:column square)))
	     (list (cons (* -1 distance) (* -1 distance))
		   (cons (* -1 distance) (*  0 distance))
		   (cons (* -1 distance) (*  1 distance))
		   (cons (*  0 distance) (* -1 distance))
		   (cons (*  0 distance) (*  1 distance))
		   (cons (*  1 distance) (* -1 distance))
		   (cons (*  1 distance) (*  0 distance))
		   (cons (*  1 distance) (*  1 distance))))))
    (filter (lambda (square)
	      (board:contains? square board))
	    (flatmap (lambda (distance)
		       (list-of-distant-squares distance square))
		     (enumerate-interval 1 (board:size board)))))

  (define (board:mark-attacked! square-list board)
    (if (null? square-list)
	board
      (begin (square-array:set! (square:row (car square-list))
				(square:column (car square-list))
				board
				#\.)
	     (board:mark-attacked! (cdr square-list)
				   board))))

  (square-array:set! (square:row square) (square:column square) board #\Q)

  (board:mark-attacked! (queens-attacked-squares-list square board)
			board)

  board)

(define (board:get-free-square-list board)

  (define (board:free? row column)
    (eq? #\_ (square-array:get row column board)))

  (let next-row ((rows-processed 0)
		 (result '()))

    (if (< rows-processed (square-array:size board))
	(next-row (+ 1 rows-processed)
		  (let next-column ((columns-processed 0)
				    (result result))
		    (if (< columns-processed (square-array:size board))
			(next-column (+ 1 columns-processed)
				     (if (board:free? rows-processed columns-processed)
					 (cons (square:make rows-processed
							    columns-processed)
					       result)
				       result))
		      result)))
      (reverse result))))

(define (try n delay . options)
  (define find-all (memq 'find-all options))
  (define optimization-1 (memq 'o1 options))
  (define optimization-2 (memq 'o2 options))
  (define calls 0)

  (define (try-queens n board population)

    (define (indented-display thing) (let loop ((spaces 0))
				       (if (= spaces population)
					   (display thing)
					 (begin (display " ")
						(loop (+ 1 spaces))))))

    (set! calls (+ 1 calls))
    (begin
      (display "\n\n")
      (indented-display "{")
      (display calls)
      (display " trying to place ")
      (display n)
      (display " queens on this board, which already has ")
      (display population)
      (display "\n")
      (display (board:->string board))
      (sleep delay))

    (let ((return-value (if (= n 0)
			    (list board)

			  ;; Try to place n queens on the board.

			  ;; For each free square on the board...
			  (let next-free-square ((free-squares (filter (if optimization-1
									   (lambda (square)
									     (= (- (board:size board) n)
										(square:column square)))
									 (lambda (x)
									   x))

								       (board:get-free-square-list
									board)))
						 (result '()))

			    (if
				(if optimization-2
				    (< (length free-squares)
				       n)
				  (null? free-squares))

				(begin
				  (if (null? result)
				      (begin
					(indented-display "Can't place ")
					(display  n)
					(display " queens\n")))

				  result)

			      ;; Place a queen on this square.

			      ;; Now try to place n-1 queens on the board.  If we succeed, add
			      ;; the board to our list of solutions.
			      (let* ((solutions (try-queens (- n 1)
							    (board:occupy! (car free-squares)
									   (board:copy board))
							    (+ 1 population))))

				(if (and
				     (not find-all)
				     (not (null? solutions)))

				    (adjoin solutions result)

				  (next-free-square (cdr free-squares)
						    (adjoin solutions result))
				  )

				))))))
      (indented-display "}\n")
      return-value
      ))

  (try-queens n (board:make n) 0))