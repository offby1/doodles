;; this is something that's hard to do in Perl -- writing to, and
;; reading from, the same subprocess at once (run 'perldoc IPC::Open2'
;; for an explanation of how to do it in Perl).  I'm not actually sure
;; that this technique is foolproof -- this program has worked the few
;; times I've run it, but multi-threaded stuff is tricky enough that
;; I'm still not convinced.  Still, it seems reasonable.

;; The problem that I'm afraid of is deadlock.  Eli Barzilay says this
;; technique (using separate threads to read and write) is fine.

(define (writer-proc port)
  (let loop ((lines-written 0))
    (when (< lines-written 100)
      (let ((datum (random 10)))
        (display datum port)
        (newline port)
        (fprintf (current-error-port) "Wrote ~s~%" datum)
        (loop (add1 lines-written)))))
  (close-output-port port))

(define (reader-proc port)
  (let loop ()
    (let ((datum (read-line port)))
      (when (not (eof-object? datum))
        (fprintf (current-error-port) "Got ~s~%" datum)
        (loop))))
  (close-input-port port))

(define (find-exe-or-die fn)
  (or (find-executable-path fn)
      (find-executable-path (string-append fn ".exe"))
      (raise-user-error 'find-exe-or-die "Couldn't find executable ~s" fn)))

(let ((program-name "grep"))
  (let-values (((proc readme writeme errors)
                (subprocess
                 #f
                 #f
                 (current-error-port)
                 (find-exe-or-die program-name)
                 "7")))
    (let ((reader-thread  (thread (lambda () (reader-proc readme))))
          (writer-thread  (thread (lambda () (writer-proc writeme)))))
      (subprocess-wait proc)
      (when (not (zero? (subprocess-status proc)))
        (raise-user-error
         'subprocess
         "~s returned non-zero exit status: ~a"
         program-name
         (subprocess-status proc)))

      ;; There are three waitable things here -- the subprocess, and
      ;; the two threads.  It's not clear which subset of those three
      ;; I should wait on.

      ;; It probably doesn't hurt to wait on these, although they're
      ;; probably always going to be ready, since the subprocess has
      ;; exited.
      (for-each thread-wait (list writer-thread reader-thread)))))
