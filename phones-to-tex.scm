(let ()
  (define file->string

    (let ()
      (define bytes-per-character 1)	; in most scheme
                                        ; implementations.  Might be 2
                                        ; for an implementation that
                                        ; understood UNICODE.
  
      (lambda (filename)
	(call-with-input-file
	    filename 
	  (lambda (port)
	    (define loop

	      (let ((result 

		     ;; There's no guarantee that we will read exactly
		     ;; this many characters -- we might read more or
		     ;; fewer.  Still, it's a reasonable first guess.

		     (make-string (/ (stat:size (stat filename))
				     bytes-per-character
				     ))))

		(lambda (chars-set)
		  (if (eof-object? (peek-char port))
                    
		      ;; sometimes there are fewer characters in a file
		      ;; than `stat' reports.  This happens, for example,
		      ;; on Cygwin32 under Windows NT -- `stat' includes
		      ;; carriage returns in its count, but read-char
		      ;; doesn't return them.  In those cases, our string
		      ;; will have space for more characters than we
		      ;; actually read, so we need to truncate the string.
		      ;; A quick way to do that is to return a shared
		      ;; substring.
		      (make-shared-substring result 0 chars-set)
                                                              
		    (begin

		      (if (= chars-set (string-length result))

			  ;; We're out of room.  Perhaps the file grew
			  ;; since we last called `stat', or perhaps
			  ;; `stat' simply didn't report an accurate
			  ;; value.

			  ;; Make more room.

			  ;; The new string is one more than twice as many
			  ;; characters long.
			  (set! result (string-append result (make-string (+ 1 (string-length result)) #\?))))

		      (string-set! result chars-set (read-char port))
		      (loop (+ 1 chars-set)))))))
	    (loop 0))))))

  (define Phones (file->string (string-append
				(getenv "HOME")
				"/Phones")))

  (define (quote-tex-special-chars str)
    (let ((return (make-string (* 2 (string-length str)))))
      (let loop ((chars-read 0)
		 (chars-written 0))
	(if (= chars-read (string-length str))
	    (make-shared-substring return 0 chars-written)
	  (let ((this-char (string-ref str chars-read)))
	    (case this-char
	      ((#\\ #\{ #\} #\$ #\& #\# #\^ #\_ #\% #\~)
	       (string-set! return chars-written #\\)
	       (set! chars-written (+ 1 chars-written))))
	   
	    (string-set! return chars-written this-char)

	    (loop (+ 1 chars-read)
		  (+ 1 chars-written)))))))

     (with-output-to-file (string-append
			   (getenv "HOME")
			   "/Phones.tex")
       (lambda ()
	 (display (quote-tex-special-chars Phones)))))
