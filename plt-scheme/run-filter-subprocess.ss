(module run-filter-subprocess mzscheme

;; this is something that's hard to do in Perl -- writing to, and
;; reading from, the same subprocess at once (run 'perldoc IPC::Open2'
;; for an explanation of how to do it in Perl).

(require (lib "process.ss")
         (lib "match.ss"))

;; I'm not certain that using "process*" from process.ss works any
;; better than using the "process" that's built in to mzscheme.
(define (noisily-find-exe-path fn)
  (or (find-executable-path fn)
      (raise-user-error 'find-executable-path "Couldn't find executable ~s" fn)))

(let ((program-name (noisily-find-exe-path "sort")))
  (match-let (((readme writeme pid err controller)
               (process* program-name "-n")))

    (fprintf (current-error-port)
             "Started ~a, PID ~s; status is ~s~%"
             program-name
             pid
             (controller 'status))
    (when (eq? 'running (controller 'status))
      ;; write some data.
      (let loop ((lines-written 0))
        (when (< lines-written 10)
          (let ((datum (random 10000000)))
            (display datum writeme)
            (newline writeme)
            (fprintf (current-error-port) "Wrote ~s~%" datum)
            (loop (add1 lines-written))))
        (close-output-port writeme))

      ;; read the processed data.
      (let loop ()
        (let ((datum (read-line readme)))
          (when (not (eof-object? datum))
            (fprintf (current-error-port) "Got ~s~%" datum)
            (loop)))
        (close-input-port readme))

       (controller 'wait))

    (case (controller 'status)
      ((running)
       (fprintf (current-error-port)
                "hmm, for some reason ~a is still running~%"
                program-name)
       (controller 'kill))
      ((done-ok)
       (fprintf (current-error-port)
                "Ain't life grand?~%"))
      ((done-error)
       (raise-user-error
        'subprocess
        "~s returned a failure exit status: ~a"
        program-name
        (controller 'exit-code))))

    ;; just for tidyness
    (close-input-port err)
    )))

