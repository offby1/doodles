(module shuffle-client mzscheme
(define *times-per-thunk* 100)
(define *threads* 100)

(define *completed-items* 0)
(define *accountants-inbox* (make-channel))
(define *accountant*
  (thread
   (lambda ()
     (let loop ()
       (let ((completed-thingy (channel-get *accountants-inbox*)))
         (set! *completed-items* (add1 *completed-items*))
         (loop))))))

(define spam-thunk
  (lambda ()
    (file-stream-buffer-mode (current-error-port) 'line)
    (let again ((wait-seconds 1))
      (with-handlers
          ([exn:fail:network?
            (lambda (e)
              (if (regexp-match #rx"refused" (exn-message e))
                  (fprintf (current-error-port)
                           "aw, it refused us~%")
                  (begin
                    (fprintf (current-error-port)
                             "hmm, ~a~%"
                             (exn-message e))
;;;                     (sleep wait-seconds)
;;;                     (again (* 2 wait-seconds))
                    )))])

        (let loop ((times 0))
          (when (< times *times-per-thunk*)
            (let-values (((ip op)
                          (tcp-connect/enable-break "192.168.0.126" 1122)))
              (close-output-port op)
              (let ((datum (read ip)))
                (cond
                 ((vector? datum)
                  (channel-put *accountants-inbox* datum))
                 (else
                  (printf "??!!~%"))))
              (close-input-port ip))
            (loop (add1 times))))))))

(for-each sync
          (let loop ((threads-to-make *threads*)
                     (threads '()))
            (if (positive? threads-to-make)
                (begin
                  (printf "~a~%" threads-to-make) (flush-output)
                  (loop (sub1 threads-to-make)
                        (cons (thread spam-thunk )
                              threads)))
                threads)))
(kill-thread *accountant*)
(printf "We got ~a completed shuffled thingies~%"
        *completed-items*)
)