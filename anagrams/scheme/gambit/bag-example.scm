(with-input-from-file "/usr/share/dict/words"
  (lambda ()
    (let loop ((word  (read-line))
               (words-read 0))
      (if (not (eof-object? word))
          (begin
            (display word)
            (display ": ")
            (display (bag word))
            (newline)
            (loop (read-line)
                  (+ 1 words-read)))
        ))
    ))