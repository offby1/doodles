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
      (let ((datum (format "Snurkly #~a!~%" lines-written)))
        (display datum port)
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

(let-values (((proc readme writeme errors)
              (subprocess #f #f (current-error-port)
                          (find-executable-path "tac"))))
  (let ((reader-thread  (thread (lambda () (reader-proc readme))))
        (writer-thread  (thread (lambda () (writer-proc writeme)))))

    (for-each thread-wait (list writer-thread reader-thread))

    )))
