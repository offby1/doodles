(module run-filter-subprocess mzscheme

;; this is something that's hard to do in Perl -- writing to, and
;; reading from, the same subprocess at once (run 'perldoc IPC::Open2'
;; for an explanation of how to do it in Perl).

(define (noisily-find-exe-path fn)
  (or (find-executable-path fn)
      (raise-user-error 'find-executable-path "Couldn't find executable ~s" fn)))

(let ((program-name (noisily-find-exe-path "sort")))
  (let-values (((pid readme writeme err)
                (subprocess #f #f #f program-name "-n")))

    (fprintf (current-error-port)
             "Started ~a, PID ~s; status is ~s~%"
             program-name
             pid
             (subprocess-status pid))
    (when (eq? 'running (subprocess-status pid))
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

       (subprocess-wait pid))

    (case (subprocess-status pid)
      ((running)
       (fprintf (current-error-port)
                "hmm, for some reason ~a is still running~%"
                program-name)
       )
      ((0)
       (fprintf (current-error-port)
                "Ain't life grand?~%"))
      (else
       (raise-user-error
        'subprocess
        "~s returned a failure exit status: ~a"
        program-name
        (subprocess-status pid))))

    ;; just for tidyness
    (close-input-port err)
    )))

