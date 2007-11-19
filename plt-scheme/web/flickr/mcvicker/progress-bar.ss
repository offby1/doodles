(module progress-bar mzscheme
(require
 (lib "class.ss")
 (lib "mred.ss" "mred"))

;; this class inherits from "dialog%"

;; it has a button with a callback, and a "gauge".

;; the constructor takes a callback for the button, and a positive
;; integer for the amount of work to be done; it returns the dialog,
;; and a thunk to advance the gauge.

;; it has a couple of procedures to fiddle the gauge.

(file-stream-buffer-mode (current-error-port) 'line)

(define (make-progress-bar parent cancel-callback work-to-do)
  (let* ((d (new dialog%
                 (label "Progress!")))
         (b (new button%
                 (label "Cancel")
                 (parent d)
                 (callback cancel-callback)))
         (g (new gauge%
                 (label #f)
                 (range work-to-do)
                 (parent d))
            ))
    (values
     d
     (lambda ()
       (fprintf (current-error-port)
                "Gauge's value is ~a~%" (send g get-value))
       (send g set-value (add1 (send g get-value)))))))

(define-values  (pb advance!)
  (make-progress-bar
   #f
   (lambda (item event)
     (fprintf (current-error-port)
              "Ow!~%"))
   10))

(thread
 (lambda ()
   (let loop ((x 0))
     (when (< x 5)
       (advance!)
       (sleep 1/2)
       (loop (add1 x))))))

(send pb show #t)
)