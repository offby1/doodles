(define (bargraph str)

  (define stats
    (let ((chars (make-vector char-set-size 0)))
      (define (vector-increment! v k)
        (let ((old (vector-ref v k)))
          (vector-set! v k (+ old 1))))
      (let loop ((chars-processed 0))
        (if (= chars-processed (string-length str))
            chars
          (begin
            (vector-increment! chars (my-char->integer (string-ref str chars-processed)))
            (loop (+ 1 chars-processed)))))))

  (with-output-to-file "/tmp/stats"
    (lambda ()
      (let loop ((slots-processed 0))
        (if (< slots-processed char-set-size)
            (begin
              (display slots-processed)
              (display " ")
              (display (vector-ref stats slots-processed))
              (newline)
              (loop (+ 1 slots-processed))))))))