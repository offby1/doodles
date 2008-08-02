#lang scheme

;; dd if=/dev/zero bs=1 count=10000 | gzip | wc -c
;;
;; => 45

(require file/gzip)

(define (drain-and-count ip)
  (let ((seen 0))
    (for ((b (in-input-port-bytes ip)))
      (set! seen (add1 seen)))
    seen))

(define *ten-thousand-nulls* (make-bytes 10000))

(define deflated (do deflate *ten-thousand-nulls*))

(printf "~a bytes in; ~a bytes out~%"
        (bytes-length *ten-thousand-nulls*)
        (bytes-length deflated))

(define (size-when-deflated-via-the-built-in-thingy ip)
  (drain-and-count (deflate ip)))

;; This sure seems klunky, although it gets the job done.
(define (size-when-deflated-via-gzip ip)
  (define-values (proc stdout-ip stdin-op stderr-ip)
    (subprocess #f ip #f "/bin/gzip" "-v"))
  (define error-reporter
    (thread
     (lambda ()
       (for ((error-line (in-lines stderr-ip)))
         (display error-line (current-error-port))
         (newline (current-error-port)))
       (close-input-port stderr-ip))))

  (begin0
      (drain-and-count stdout-ip)
    (close-input-port stdout-ip)
    (thread-wait error-reporter)))
