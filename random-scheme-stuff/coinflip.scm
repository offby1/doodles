#!/usr/bin/guile -s
!#

(use-modules (ice-9 slib))
(use-modules (ice-9 popen))

(define (random-char)
  (with-input-from-file "/dev/urandom"
    (lambda ()
      (read-char))))

;; From SLIB
(require 'logical)

(define (count-set-bits byte)
  (logcount (char->integer byte)))

(define (flip-eight-times)
  (count-set-bits (random-char)))

(define (flip-many-times n)
  (if (not (zero? (modulo n 8)))
      (error "N must be a multiple of 8, but is" n))
  (let loop ((calls-to-make (/ n 8))
             (sum 0))
    (if (zero? calls-to-make)
        sum
      (loop (- calls-to-make 1)
            (+ sum (flip-eight-times))))))

(define (increment! n)
  (let ((old (vector-ref stats n)))
    (vector-set! stats n (+ 1 old))))

(begin
  (define stats (make-vector 160 0))
  (define trials-to-do 200)
  (define (vector->gnuplot-data v)
    (let loop ((slots-plotted 0)
               (return ""))
      (if (= slots-plotted (vector-length v))
          return
        (loop (+ 1 slots-plotted)
              (string-append 
               return 
               (number->string slots-plotted)
               " "
               (number->string (vector-ref v slots-plotted))
               "\n")))))
  (let loop ((trials trials-to-do))
    (if (zero? trials)
        stats
      (begin
        (increment! (flip-many-times (vector-length stats)))
        (loop (- trials 1)))))
  
  (let* ((command-line (string-append 

                        ;; these arguments work for GNU Plotutils 2.3

                        ;; -T X: display in an X window
                        ;; -m -1 : don't draw lines between points,
                        ;; instead draw ...
                        ;; -S 5 ... little crosses

                        "graph -T X -m -1 -S 5"
                        " -X 'Number of Heads'"
                        " -Y 'Number of trials with that many Heads'"
                        " -L '" (number->string trials-to-do) " trials of " (number->string (vector-length stats)) " flips each'"
                        ))
         (plot (open-output-pipe command-line)))
    (display (vector->gnuplot-data stats) plot)
    (close-pipe plot)))
