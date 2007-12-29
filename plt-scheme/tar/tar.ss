#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui tar-tests 'verbose))"
|#
(module tar mzscheme
(require (lib "serialize.ss")
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

(define (file-contents p)
  (call-with-input-file p
    (lambda (ip)
      (let loop ((result #""))
        (let ((chunk (read-bytes 4096 ip)))
          (if (eof-object? chunk)
              result
              (loop (bytes-append result chunk))))))))

(define-serializable-struct file-info (path contents is-file? is-link? file-or-directory-modify-seconds file-or-directory-permissions)
  #f)
(define-serializable-struct directory-info (path file-or-directory-modify-seconds file-or-directory-permissions children)
  #f)

(define (path->info p)
  (cond
   ((directory-exists? p)
    (make-directory-info
     p
     (file-or-directory-modify-seconds p)
     (file-or-directory-permissions p)
     (map (lambda (child)
            (path->info (build-path p child)))
          (directory-list p))))
   ((or (file-exists? p)
        (link-exists? p))
    (make-file-info
     p
     (file-contents p)
     (file-exists? p)
     (link-exists? p)
     (file-or-directory-modify-seconds p)
     (file-or-directory-permissions p)))
   (#t
    (raise (make-exn:fail:filesystem
            (format "path ~s is not a directory, file, or link" p)
            (current-continuation-marks))))))

(define (summarize-string s)
  (let* ((l (string-length s))
        (to-show (min l 10)))
    (string-append
     (substring s 0 to-show)
     (if (< to-show l )
         " ... "
         ""))))

(define tar-tests

  (test-suite
   "tar"
   (test-case
    "c:/autoexec.bat"
    (let* ((name  "c:/AUTOEXEC.bat")
           (i  (path->info name)))
      (check-true (zero? (bytes-length (file-info-contents i))))
      (check-equal? (file-info-path i) name)))
   (test-case
    "/snorklebutt"
    (check-exn exn:fail:filesystem? (lambda () (path->info "/snorklebutt"))))
   (test-case
    "directory"
    (write (summarize-string (format "~a" (serialize (path->info ".")))))
    (newline))
   ))

(provide tar-tests)
)
