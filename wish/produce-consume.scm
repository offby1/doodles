(require 'queue)
(let ()
  (define buffer (make-queue))

  (define (continue what-to-do-when-blocked)
    (call-with-current-continuation
     (lambda (cc)
       (what-to-do-when-blocked cc)
       cc)))

  (define (produce what-to-do-when-blocked)
    (define (put-in-buffer! obj)

      (if (not (queue-empty? buffer))
          (set! what-to-do-when-blocked (continue what-to-do-when-blocked)))

      (enqueue! buffer obj)
      (enqueue! buffer 'padding)
      (display-many "Put `" obj "' in buffer" #\newline))

    (let loop ((n 0))
      (put-in-buffer! n)
      (loop (+ n 1))))

  (define (consume what-to-do-when-blocked)
    (define (get-from-buffer!)

      (if (queue-empty? buffer)
          (set! what-to-do-when-blocked (continue what-to-do-when-blocked)))

      (if (queue-empty? buffer)
          #f
        (let ((result (dequeue! buffer)))
          (display-many "Got `" result "' from buffer"
                        #\newline)
          result)))

    (get-from-buffer!)
    (get-from-buffer!)
    (get-from-buffer!)
    (get-from-buffer!)
    (get-from-buffer!)
    (get-from-buffer!)

    (display-many "Consumer is done." #\newline))

  ;;(trace produce)
  ;;(trace consume)
  (consume produce))

