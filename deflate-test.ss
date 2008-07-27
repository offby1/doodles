#lang scheme

;; dd if=/dev/zero bs=1 count=10000 | gzip | wc -c
;;
;; => 45

(require file/gzip)

(define (do proc b)
  (let ((ip (open-input-bytes b))
        (op (open-output-bytes)))
    (proc ip op)
    (get-output-bytes op)))

(define *ten-thousand-nulls* (make-bytes 10000))

(define deflated (do deflate *ten-thousand-nulls*))

(printf "~a bytes in; ~a bytes out~%"
        (bytes-length *ten-thousand-nulls*)
        (bytes-length deflated))
