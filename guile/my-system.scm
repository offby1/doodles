(define (quote-for-posix-shell str)
  (define (maybe-escape ch)
    
    (define (needs-quoting?)
      (string-index "!|&;()<>'\| \t?*" ch))

    (if (needs-quoting?)
        (list #\\ ch)
      (list ch)))

  (let loop ((chars-examined 0)
             (chars '()))
    (if (= chars-examined (string-length str))
        (list->string chars)
      (loop (+ 1 chars-examined)
            (append chars
                    (maybe-escape (string-ref str
                                              chars-examined)))))))


;; Here's how to run a command and capture its output.
(let ((pp (open-input-pipe "ls /")))
  (let loop ((lines '())
             (this-line (read-line pp)))
    (if (not (eof-object? this-line))
        (loop (cons this-line lines)
              (read-line pp))
      (reverse lines))))


;; Here's how to figure out why a system call failed:
(catch
 'system-error
 (lambda ()
   (mkdir 
    "/this-ought-to-fail-if-I'm-not-root"))
 (lambda stuff
   (let ((errno (car (list-ref stuff 4))))
     (cond
      ((= errno EACCES)
       (display "You're not allowed to do that."))
      ((= errno EEXIST)
       (display "Already exists."))
      (#t
       (display (strerror errno))))
     (newline))))


;; Like the `system' that comes with Guile, but it takes a list of
;; command arguments, á la execlp (which in fact we use).  Thus we
;; avoid all the hairy shell-syntax quoting problems.

;; Don't pass the program name twice like this: (list "ls" "ls" "-l"
;; "~").
(define (my-system arglist)
  (let ((child (primitive-fork)))
    (if (zero? child)
        (apply execlp (cons (car arglist) arglist))
      (cdr (waitpid child)))))