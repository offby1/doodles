(require 'filter)

(define (file->expressions filename)
         (call-with-input-file filename
           (lambda (port)
             (let loop ((expression (read port))
                        (result '()))
               (if (eof-object? expression)
                   (reverse result)
                 (loop (read port)
                       (cons expression result)))))))

(defmacro trace-em (filename)
  (cons
   'trace
   (map caadr
        (filter (lambda (form)
                  (and (pair? form)
                       (eq? 'define (car form))
                       (list? (cadr form))))
                (file->expressions filename)))))

(load "eval10.scm")
(trace-em "eval10.scm")

;;(use-modules (eric eval))

(display "Evaluating.\n")

(let loop ((expressions (file->expressions "test.stupid")))
  (if (not (null? expressions))
      (begin
        (newline)
        (display "---> Evaluating `")
        (display (car expressions))
        (display "': ")
        (display (my-eval (car expressions)))
        (newline)

        ;; To clear out those pairs whose status is
        ;; `required-for-current-eval'
        (collect-garbage)

        (display "Pairs:\n")
        (display (pairs-as-string #f))
        (newline)
        (display "Frames:\n")
        (display (summarize-frames))
        (newline)
        (loop (cdr expressions)))))
