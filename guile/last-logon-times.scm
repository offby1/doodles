#!/usr/local/bin/guile -s
!#

;; Guess when the user last logged on, by examining the last-mod times
;; of some files in their home directory.
;; The returned time is a 
(define (last-logon-time u)
  (let ((homedir (passwd:dir u))
        (when 0))
    (for-each
     (lambda (fn)
       (let ((stat-info (false-if-exception (stat (string-append homedir "/" fn)))))
         (if stat-info
             (if (> (stat:mtime stat-info) when)
                 (set! when (stat:mtime stat-info))))))
     (list ".bash_history" ".guile_history" ".history"))
     when))

(display
 (apply string-append
        (map (lambda (l)
               (string-append 
                (let ((seconds (cadr l)))
                  (if (zero? seconds)
                      "never"
                    (strftime "%s (%c)" (localtime seconds))))
                ": "
                (car l)
                "\n"))
             (dynamic-wind
              setpwent
              (lambda ()
                (let loop ((user (getpwent))
                           (result '()))
                  (if (not user)
                      result
                    (loop (getpwent)
                          (cons (list (passwd:name user)
                                      (last-logon-time user))
                                result)))))
              endpwent))))
