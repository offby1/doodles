(require 'common-list-functions)	; for reduce
(require 'format)
(require 'sort)
(require 'random)

(define falsehoods '())

(define options '())

(define alist-surveys (not (memq 'vector-surveys options)))

(define (make-alist-survey)

  (let ((value '()))
    (define (survey:add! s c)
      (if (null? s)
	  (list (cons c 1))
	(let ((old-value (assq c s)))
	  (if old-value
	      (begin
		(set-cdr! old-value (+ 1 (cdr old-value)))
		s)
	    (cons (cons c 1)
		  s)))))

    (lambda (op . args)
      (case op
	((non-zero-elements)
	 (length value))

	((add)
	 (set! value (survey:add! value (car args))))

	((ref)
	 (let ((cell (assq (car args) value)))
	   (if cell
	       (cdr cell)
	     0)))

	((expose)

	 (sort value
	       (lambda (foo bar)
		 (char<? (car foo)
			 (car bar)))))

	(else
	 (error "Unknown op for survey: " op))))))

(define survey:make (lambda ()
		    (let ((return (make-alist-survey)))
		      ;(trace return)
		      return)))

(define survey:expose (lambda (s)
		      (s 'expose)))

(define survey:size (lambda (s)
		    (s 'size)))

(define survey:add! (lambda ( s c)
		    (s 'add c)))

(define survey:ref (lambda ( s c)
		   (s 'ref c)))

(define (make-doublet thing1 thing2)

  (let ((value (cons thing1 thing2)))
    (lambda ()
      thing1)))

(define add-to-falsehoods!
  (let ((calls 0))
    (lambda (datum)
      (display calls)
      (display "\r")
      (set! falsehoods (cons datum falsehoods))
      (set! calls (+ 1 calls)))))

(define (present? collection datum)
  (member datum collection))

(define (string->survey s)
  (let ((length (string-length s))
	(the-survey (survey:make)))

    (let loop ((chars-processed 0))
      (if (< chars-processed length)
	  (begin
	    (survey:add! the-survey (char-downcase (string-ref s (- length chars-processed 1))))
	    (loop (+ 1 chars-processed)))))
    the-survey))

(define (number-spelling n)
  (format "~R" n))

(define (character-spelling c number)
  (define (pluralize thing)
    (cond ((string? thing)
	   (string-append thing (if (= number 1)
				    ""
				  (string-append

				   (if (and
					(= 1 (string-length thing))
					(char-alphabetic? (string-ref thing 0)))
				       "'"
				     "")
				   "s"))))
	  ((pair? thing)
	   (if (= number 1)
	       (car thing)
	     (cdr thing)))
	  (else
	   (error
	    "Pluralize doesn't know how to deal with "
	    thing))))
  (pluralize
   (cond
    ((or (char-alphabetic? c)
	 (char-numeric? c))
     (string c))
    (else
     (case c
       ((#\space)
	"space")
       ((#\newline)
	"newline")
       ((#\tab)
	"tab")
       ((#\!)
	"exclamation point")
       ((#\")
	"double-quote")
       ((#\#)
	"pound sign")
       ((#\$)
	"dollar sign")
       ((#\%)
	"percent sign")
       ((#\&)
	"ampersand")
       ((#\')
	;; "single-quote"
	"apostrophe"
	)
       ((#\()
	'("left parenthesis" . "left parentheses"))
       ((#\))
	'("right parenthesis" .  "right parentheses"))
       ((#\*)
	"asterisk")
       ((#\+)
	"plus sign")
       ((#\,)
	"comma")
       ((#\-)
	"hyphen")
       ((#\.)
	;; "full stop"
	"period")
       ((#\/)
	'("slash" . "slashes"))
       ((#\:)
	"colon")
       ((#\;)
	"semi-colon")
       ((#\<)
	"less-than sign")
       ((#\=)
	"equals sign")
       ((#\>)
	"greater-than sign")
       ((#\?)
	"question mark")
       ((#\@)
	"at sign")
       ((#\[)
	"left bracket")
       ((#\\)
	'("backslash" . "backslashes"))
       ((#\])
	"right bracket")
       ((#\^)
	"caret")
       ((#\_)
	"underscore")
       ((#\`)
	"acute accent")
       ((#\{)
	"left curly-brace")
       ((#\|)
	"vertical bar")
       ((#\})
	"right curly-brace")
       ((#\~)
	"tilde")
       (else
	"goofy character"))))))

(define (template->initial-string t)
  (if (string? t)
      t
    (let loop ((t t)
	       (result ""))
      (if (null? t)
	  result
	(loop (cdr t)
	      (let* ((initial-value (lambda ()
				      (random 40)))
		     (init (initial-value)))
		(string-append result (if (string? (car t))
					  (car t)
					(string-append (number-spelling init)
						       " "
						       (character-spelling (car t)
									   init))))))))))
(define (survey->string srv template)
  (if (string? template)
      (let loop ((chars-processed 0)
		 (char-count-spellings '()))

	(define (enumerate string-list)
	  (case (length string-list)
	    ((0)
	     "")
	    ((1)
	     (car string-list))

	    ((2) (string-append (car string-list) " and " (cadr string-list)))

	    (else
	     (string-append (reduce string-append (map (lambda (s)
							 (string-append s ", "))
						       (butlast string-list 1)))
			    "and "
			    (car (last string-list 1))))))
	(if (= chars-processed char-code-limit)
	    (string-append template
			   (enumerate char-count-spellings)
			   ".")

	  (let ((this-char-count (survey:ref srv (integer->char chars-processed))))

	    (loop (+ 1 chars-processed)
		  (append char-count-spellings
			  (if (> this-char-count 0)

			      (list (string-append
				     (number-spelling this-char-count)
				     " "
				     (character-spelling (integer->char chars-processed) this-char-count)))
			    '()))))))

    (let loop ((template template)
	       (return ""))
      (if (null? template)
	  return

	(loop (cdr template)
	      (string-append return (if (string? (car template))
					(car template)
				      (string-append (number-spelling (survey:ref srv (car template)))
						     " "
						     (character-spelling (car template)(survey:ref srv (car template)) )))))))))

(define (start-from seed)

  (let loop ((s (template->initial-string seed))
             (surv (string->survey (template->initial-string seed)))
             (falsehoods-discovered 0))

    (let ((doublet (survey:expose surv)))
      
      (if (present? falsehoods doublet)
          falsehoods-discovered
        (let ((next (survey->string surv seed)))
          (if (string=? s next)
              (begin
                (display s)
                (newline)
                s)
            (begin
              (add-to-falsehoods! doublet)
              (loop next
                    (string->survey next)
                    (+ 1 falsehoods-discovered)))))))))

(define (grind seed)
  (let ((result (start-from seed)))
    (if (not (string? result))
	(grind seed)
      (begin

	(display "Hey, I found one!\n")
	(display result)
	(newline)
	result))))

(define seed
  (list "This sentence honoring Katie Drake contains exactly "
	;; #\  ", "
	;;	 #\! ", "
	;;	 #\" ", "
	;;	 #\# ", "
	;;	 #\$ ", "
	;;	 #\% ", "
	;;	 #\& ", "
	;;	 #\( ", "
	;;	 #\) ", "
	;;	 #\* ", "
	;;	 #\+ ", "
	;;	 #\/ ", "
	;;	 #\0 ", "
	;;	 #\1 ", "
	;;	 #\2 ", "
	;;	 #\3 ", "
	;;	 #\4 ", "
	;;	 #\5 ", "
	;;	 #\6 ", "
	;;	 #\7 ", "
	;;	 #\8 ", "
	;;	 #\9 ", "
	;;	 #\: ", "
	;;	 #\; ", "
	;;	 #\< ", "
	;;	 #\= ", "
	;;	 #\> ", "
	;;	 #\? ", "
	;;	 #\@ ", "
	;;	 #\[ ", "
	;;	 #\\ ", "
	;;	 #\] ", "
	;;	 #\^ ", "
	;;	 #\_ ", "
	;;	 #\` ", "

	; #\a ", "
	; #\b ", "
	; #\c ", "
	; #\d ", "
	 #\e ", "
	; #\f ", "
	; #\g ", "
	; #\h ", "
	; #\i ", "
	; #\j ", "
	; #\k ", "
	; #\l ", "
	; #\m ", "
	; #\n ", "
	; #\o ", "
	; #\p ", "
	 #\q ", "
	 #\r ", "
	 #\s ", "
	; #\t ", "
	 #\u ", "
	 #\v ", "
	#\w ", "
	#\x ", "
	#\y ", "
	#\z ", "
	#\- ", "
	#\' ", "
	#\, ", "
	#\. ", and "
	#\ "."
	))