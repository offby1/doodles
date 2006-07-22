(module run-grep mzscheme

;; this is something that's hard to do in Perl -- writing to, and
;; reading from, the same subprocess at once.  I'm not actually sure
;; that this technique is foolproof -- it's worked every time I've
;; tried it, but multi-threaded stuff is tricky enough that I'm still
;; not convinced.  Still, it seems reasonable.

(define (writer-proc port)
  (let loop ((lines-written 0))
    (when (< lines-written 10)
      (display "Snurkly!" port)
      (newline port)
      (fprintf (current-error-port) "Wrote line #~a~%" lines-written)
      (loop (add1 lines-written))))
  (close-output-port port))

(define (reader-proc port)
  (let loop ()
    (let ((datum (read-line port)))
      (when (not (eof-object? datum))
        (display "Got ")
        (write datum)
        (newline)
        (loop))))
  (close-input-port port))

(let-values (((proc readme writeme errors)
              (subprocess #f #f (current-error-port)
                          (find-executable-path "grep") "u")))
  (let ((reader-thread  (thread (lambda () (reader-proc readme))))
        (writer-thread  (thread (lambda () (writer-proc writeme)))))

    (for-each thread-wait (list writer-thread reader-thread))

    )))
