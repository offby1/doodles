(define (count-commas str)
  (cond
   ((or (not (string? str))
        (zero? (string-length str)))
    0)
   ((char=? #\, (string-ref str 0))
    (+ 1 (count-commas (make-shared-substring str 1))))
   (#t
    (count-commas (make-shared-substring str 1)))))

(require 'filter)
(filter (lambda (pair)
          (not (= 25 (cdr pair))))
        (map (lambda (str)
               (cons str (count-commas str)))
             (reverse
              (let ((return #f))
                (set!
                 return
                 (call-with-input-file
                     "/windoze/address"
                   (lambda (port)
                     (generic-reader port
                                     my-read-line
                                     (lambda (x) #f)
                                     (lambda (x) x)
                                     '()
                                     (lambda (a b) (cons b a))))))
                return))))