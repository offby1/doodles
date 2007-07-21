#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module jordan mzscheme
(require (only (lib "1.ss" "srfi")
               append-map
               second)
         (only (planet "memoize.ss" ("dherman" "memoize.plt" )) define/memo))
(provide one-jordanb-quote)

(define (grep pattern ip)
  (let loop ((results '()))
    (let ((line (read-line ip)))
      (cond
       ((eof-object? line)
        (reverse results))
       ((regexp-match pattern line)
        =>
        (lambda (match)
          (loop (cons (second match) results))))
       (else (loop results))
       ))))

(define/memo (all-jordanb-quotes)
  (let* ((log-dir (build-path (find-system-path 'home-dir) "log"))
         (files  (directory-list log-dir))
         (quotes (append-map
                  (lambda (fn)
                    (let ((fn (build-path log-dir fn)))
                      (if (file-exists? fn)
                          (call-with-input-file
                              fn
                            (lambda (ip)
                              (grep #rx"(?i:<jordanb> +(let.?s.*)$)" ip)))
                        '())))
                  files)))
    quotes
    ))

(define (one-jordanb-quote)
  (let* ((all (all-jordanb-quotes))
         (l (length all))
         (r (random l)))
    (list-ref all r))
  )

)