;; A generic read function that can be used to read different kinds of
;; data from a port.  Called with suitable parameters, it can, for
;; example, read lines, Lisp objects, or pages.

;; Reads a bunch of things from PORT, accumulates the things,
;; and returns the accumulation.

;; The accumulation starts with INIT, and calls READER to get a
;; new item.  That item is transformed with PROMOTER, whose
;; output then gets sent to APPENDER along with what's been read
;; so far.  The process stops when what we read qualifies as a
;; SEPARATOR, or if we hit end of file.

;; For example, to read a line from PORT, you'd call this with
;;	READER		read-char
;;	SEPARATOR?	(lambda (char) (char=? char #\newline))
;;	PROMOTER	string
;;	INIT		""
;;	APPENDER	string-append

(define (generic-reader port reader separator? promoter init appender)
  (if (eof-object? (peek-char port))
      (peek-char port)
    (let loop ((object (reader port))
	       (so-far init))
      (cond
       ((eof-object? object)
	so-far)
       ((separator? object)
	(appender so-far (promoter object)))
       (#t
	(loop (reader port)
	      (appender so-far (promoter object))))))))

;; I suspect that building up a string by calling `string-append' for
;; each character is hideously inefficient
(define (my-read-line port)

  (generic-reader port
		  read-char
		  (lambda (char) (char=? char #\newline))
		  string
		  ""
		  string-append
		  ))

;; Returns a list of the lines in the file
(lambda (filename)
  (define snoc
    (lambda (seq thing)
      (cons thing seq)))
  (reverse
   (call-with-input-file filename
     (lambda (port)
       (generic-reader port
                       my-read-line
                       (lambda (dummy) #f)
                       identity
                       '()
                       snoc)))))

(provide 'generic-read)
