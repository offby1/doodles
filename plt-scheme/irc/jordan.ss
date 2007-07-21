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

;; TODO -- perhaps, instead of grabbing quotes from my ~/log
;; directory, I should find some public logging service that logs
;; #emacs, and grab quotes from it.  The advantage is that anyone,not
;; just me, would then be able to run this code.

;; BUGBUG -- if the quote extends across multiple lines, this only
;; picks up the first, which looks bad.
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

;; it takes about half a minute to snarf up the files and grep them,
;; so we memoize this -- so that the second and subsequent calls are
;; fast.
(define/memo (all-jordanb-quotes test-mode?)
  (let* ((log-dir (build-path (find-system-path 'home-dir) "log"))
         (files  (directory-list log-dir))
         (files (if test-mode?
                    (list (car files))
                  files))
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

(define (one-jordanb-quote test-mode?)
  (let* ((all (all-jordanb-quotes test-mode?))
         (l (length all))
         (r (random l)))
    (list-ref all r))
  )

)