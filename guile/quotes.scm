;; See which characters need to be quoted for the Bourne shell.

(define all-characters
  (let loop ((chars '()))
    (let ((l (length chars)))
      (if (< l 256)
          (loop (cons (integer->char l)
                      chars))
        chars))))
