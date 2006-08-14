(require 'grep)
(require 'dynamic-wind)

(define jet-err
  (let ((bias (expt 2 32)))
    (lambda (number)

      (define (verbose-grep s filename . options)

        (dynamic-wind
         (lambda () (display-many "Searching for string `" s "' in file `" filename "'..." #\newline))
         (lambda () (apply (lambda (options) (grep s filename options)) options))
         (lambda () (display-many "done" #\newline) (force-output))))

      (append
       (verbose-grep
        (string-append "0x"
                       (number->string (+ bias number)
                                       16))
        "d:/nt/private/ds/src/inc/edbmsg.h"
        'i)
       (verbose-grep
        (number->string number)
        "d:/nt/public/sdk/inc/ese.h"
        'i)))))
