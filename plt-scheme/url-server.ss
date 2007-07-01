#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module url-server mzscheme
(require  (lib "url.ss" "net"))
(define (process-lines ip func)
  (let loop ()
    (let ((line (read-line ip)))
      (when (not (eof-object? line))
        (func line)
        (loop)))))

(process-lines
 (current-input-port)
 (lambda (line)
   (let* ((u (string->url line))
          (scheme (url-scheme u)))
     (printf "~%--------------~%~a~%--------------~%" line)
     (printf "url-scheme: ~s~%" (url-scheme u))
     (printf "url-user: ~a~%" (url-user u))
     (printf "url-host: ~a~%" (url-host u))
     (printf "url-port: ~a~%" (url-port u))
     (printf "url-path-absolute?: ~a~%" (url-path-absolute? u))
     (printf "url-path: ~a~%" (map (lambda (p) (cons (path/param-path p)
                                                     (path/param-param p))) (url-path u)))
     (printf "url-query: ~a~%" (url-query u))
     (printf "url-fragment: ~a~%" (url-fragment u))


     (when (and scheme
                (or
                 (string=? scheme
                           "http")
                 (string=? scheme
                           "https")))
       ;; ooh, I know how to handle this.
       (printf "Point yo' web browser at ~a~%" line)
       )
     )

   ))

;; test with
;; (echo hey you; echo yo vinny; echo http://foo/bar/baz) | ./url-server.ss
)