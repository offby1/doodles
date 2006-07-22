(module run-filter-subprocess mzscheme

;; this is something that's hard to do in Perl -- writing to, and
;; reading from, the same subprocess at once (run 'perldoc IPC::Open2'
;; for an explanation of how to do it in Perl).  I'm not actually sure
;; that this technique is foolproof -- this program has worked the few
;; times I've run it, but multi-threaded stuff is tricky enough that
;; I'm still not convinced.  Still, it seems reasonable.

(define (writer-proc port)
  (let loop ((lines-written 0))
    (when (< lines-written 10)
      (let ((datum (random 10000000)))
        (display datum port)
        (newline port)
        ;; uncomment this watch the whole program hang :-
        ;;(error "Oh shit!  Something awful happened when writing!!")
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

(define (noisily-find-exe-path fn)
  (or (find-executable-path fn)
      (raise-user-error 'find-executable-path "Couldn't find executable ~s" fn)))

(let ((program-name "sort"))
  (let-values (((proc readme writeme errors)
                (subprocess
                 #f
                 #f
                 (current-error-port)
                 (noisily-find-exe-path program-name)
                 "-n")))
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
      (for-each thread-wait (list writer-thread reader-thread))))))
