(define file->string

  (lambda (filename)
    (define bytes-per-character 1)	; in most scheme
                                        ; implementations.  Might be 2
                                        ; for an implementation that
                                        ; understood UNICODE.
  

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
        (loop 0)))))
