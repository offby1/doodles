;; This is for GNU Guile 1.2!

(require 'format)
(require 'sort)
(require 'filter)

(load "canonicalize.scm")

;; returns a string describing the files in the named directory.  The
;; string is similar to the output of `ls -l'.
(define (canonical-list dirname)

  ;; Given the name of a file, returns a string containing a description
  ;; of the file, according to `stat'.  Throws whatever exceptions
  ;; `stat' throws.
  (define (stat-info-string fn)

    (let ((stat-info (stat fn)))
      (string-append
       (case (stat:type stat-info)
         ;; These characters may not exactly conform to `ls' -- I just
         ;; picked them off the top of my head (how'd they get there
         ;; in the first place?)
         ((regular)       "-")
         ((directory)     "d")
         ((symlink)       "l")
         ((block-special) "b")
         ((char-special)  "c")
         ((fifo)          "f")
         ((socket)        "s")
         (else            "?"))

       ;; generates `rwxrwxrwx', or whatever, from the permissions field.
       (let (
             ;; the permissions field as ones and zeros.  e.g. 755
             ;; -> "111101101"
             (binary-digits (format "~9,'0B" ;; BUGBUG -- I'm throwing
					     ;; away information here
					     ;; because I don't know
					     ;; how to deal with it. 
					     (remainder (stat:perms stat-info)
							#o1000)))
             )
         (let loop ((digits-whacked 0))
           (if (< digits-whacked 9)
               (begin
                 (string-set! binary-digits 
                              digits-whacked
                              (if (char=? #\1 (string-ref binary-digits digits-whacked))
                                  (string-ref "rwx" (remainder digits-whacked 3))
                                #\-))
                 (loop (+ 1 digits-whacked)))))
         binary-digits)
       " "
       (format "~3D" (stat:nlink stat-info))
       " "

       (format "~10A" (group:name (getgrgid (stat:gid stat-info))))

       " "
       (format "~10D" (stat:size stat-info))
       " "
       (strftime  "%Y-%m-%d %H:%M:%S"	; this format string isn't
                                        ; quite the same as `ls',
                                        ; but what the hell -- I
                                        ; like it
                  (localtime (stat:mtime stat-info))))))

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

  (apply string-append
         (map 
              ;; returns a descriptive string concatenated with the
              ;; file's name.
              (lambda (fn)
                (string-append 
                 
                 ;; Return stat-info-string if possible; otherwise
                 ;; return a string describing why we couldn't return
                 ;; stat-info-string. 
                 (catch
                  'system-error
                  (lambda () (stat-info-string fn))
                  (lambda (key . args)
                    (let ((message-args (caddr args)))
                      (string-append "System error: `" (car message-args)
                                     ": " (cadr message-args)
                                     "'"))))                   
                   
                 " "
                 fn
                 (string #\newline)))
 
              (map
               (lambda (entry)
                 
                 (canonicalize (string-append dirname "/" entry)))
               
               (filter (lambda (entry)
                         (and (not (string=? "." entry))
                              (not (string=? ".." entry))))
                       (dir->list dirname))))))
