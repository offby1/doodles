(define (canonical-list-of-contents file-or-dir-name)

  ;; Given the name of a directory, returns a list of strings
  ;; representing the contents of that directory.  Throws whatever
  ;; exceptions `opendir' throws.

  ;; The strings will not contain slashes.  That is, they're filenames
  ;; relative to the named directory.

  (define (dir->list dirname)
    (let ((dir (opendir dirname)))
      (let ((result
	     (let loop ((result '())
			(entry-name (readdir dir)))
	       (if (eof-object? entry-name)
		   (reverse result)
		   (loop (cons entry-name result)
			 (readdir dir))))))
	(closedir dir)
	result)))

  (if (eq? 'directory (stat:type (stat file-or-dir-name)))
      (map
       (lambda (entry)
   
	 (canonicalize (string-append file-or-dir-name "/" entry)))
 
       (filter (lambda (entry)
		 (and (not (string=? "." entry))
		      (not (string=? ".." entry))))
	       (catch
		'system-error
		(lambda () (dir->list file-or-dir-name))
		(lambda (key . args)
		  (let ((message-args (caddr args)))
		    (display
		     (string-append
		      file-or-dir-name
		      ": "
		      (car message-args)
		      (string #\newline))
		     (current-error-port)))
		  '())
		)))
    (list (canonicalize file-or-dir-name))))