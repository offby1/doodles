#!/usr/local/bin/guile -s
!#

;; displays a word that is like its first argument, but with some
;; characters replaced with underscores.  The intent is that the first
;; argument is a Unix file name, and this program displays a "better"
;; name (one that is less likely to confuse the shell, for example).

(define (better-name fn)

  (define (better-char ch)

    (define (char-ok? ch)
      (or (char-alphabetic? ch)
          (char-numeric?    ch)
          (char=?  #\.      ch       )))

    (if (not (char-ok? ch))
        #\_
      ch))

  (let ((new (make-string (string-length fn))))
    (let loop ((chars-to-process (string-length fn)))
      (if (zero? chars-to-process)
          new
        (begin
          (string-set!
           new
           (- chars-to-process 1)
           (better-char (string-ref fn (- chars-to-process 1))))
          (loop (- chars-to-process 1)))))))

(begin
  (display (better-name (list-ref (command-line) 1)))
  (newline))