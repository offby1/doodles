(module run-filter-subprocess mzscheme

;; this is something that's hard to do in Perl -- writing to, and
;; reading from, the same subprocess at once (run 'perldoc IPC::Open2'
;; for an explanation of how to do it in Perl).

;; However, this doesn't really demonstrate anything useful -- I was
;; hoping it would demonstrate that "subprocess" magically avoids
;; deadlock, but because I'm sending "sort" a ton of input, then
;; closing its input and reading all its input, there's no risk of
;; deadlock.

;; For a while I thought that the "process" procedure from
;; "process.ss", which looks supercifically similar to "subprocess",
;; did a better job, but it's now clear that I don't understand what
;; the difference is between the two procedures.  (I've begged Eli
;; Barzilay to improve the doc for process.ss to make the difference
;; clear)

;; in any case, he advises using a separate thread for each port, to
;; be sure to avoid deadlock:

;; <elibarzilay> If you always start a thread, then it doesn't matter which facility you use.

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
        (when (< lines-written 10000)
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

