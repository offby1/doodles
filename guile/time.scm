;; This expression will cause Scheme to barf unless you've prepared it for macros by doing the following:
;;   (require 'macro)

(define-syntax time
  (syntax-rules ()
                ((time expr)
                 (let* ((start (gettimeofday))
                        (result expr)
                        (stop (gettimeofday)))
                   (display "time `")
                   (display 'expr)
                   (display "': ")
                   (display (+ (- (car stop) (car start))
                               (/ (- (cdr stop) (cdr start))
                                  1000000)))
                   (newline)
                   result))))
