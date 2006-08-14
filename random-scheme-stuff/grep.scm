;; Simple-minded `grep'

(require 'generic-read)

(define (grep s filename . options)

  (define (contains? line s case-sensitive?)

    (define (is-proper-prefix? short long case-sensitive?)
      ((if case-sensitive?
	   string=?
	 string-ci=?) short (substring long 0 (string-length short))))

    (cond
     ((> (string-length s)
	 (string-length line))
      #f)
     ((is-proper-prefix? s line case-sensitive?)
      #t)
     (#t
      (contains? (substring line 1 (string-length line))
		 s
		 case-sensitive?))))

  (let ((case-sensitive? (not (memq 'i options))))

    (call-with-input-file filename
      (lambda (port)
	(let loop ((line (my-read-line port))
		   (results '()))
	  (if (eof-object? line)
	      results
	    (loop (my-read-line port)
		  (if (contains? line s case-sensitive?)
		      (append results (list

				       ;; Get rid of newline
				       (substring line 0 (- (string-length line)
							    1))

				       ))
		    results))))))))
(provide 'grep)