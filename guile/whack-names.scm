#!/usr/bin/guile -s
!#

(define (dir->list)
  (let ((dir (opendir ".")))
    (let ((result
           (let loop ((result '())
                      (entry-name (readdir dir)))
             (if (eof-object? entry-name)
                 (reverse result)
               (loop (cons entry-name result)
                     (readdir dir))))))
      (closedir dir)
      result)))

(define (maybe-rename-file fn)
  (define (better-name fn)
    (let ((result (make-string (string-length fn))))
      (let loop ((chars-remaining (string-length fn)))
        (if (zero? chars-remaining)
            result
          (let ((this-char (string-ref fn (- chars-remaining 1))))
            (define (good-char? ch)
              (or (char-alphabetic? ch)
                  (char-numeric? ch)
                  (memq ch (list #\_ #\- #\+ #\. #\, #\= #\;))))
            (string-set!
             result
             (- chars-remaining 1)
             (if (not (good-char? this-char))
                 #\_
               this-char))
            (loop (- chars-remaining 1)))))))
  
  (let ((better (better-name fn)))
    (if (not (string=? better fn))
        (begin
          (rename-file fn better)
          (write fn)
          (display "->")
          (write better)
          (newline)))))

(for-each
 maybe-rename-file
 (dir->list))
