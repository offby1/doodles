(define (sansextension input-file-name)
  (let* ((file-name-non-directory-regexp (make-regexp "[^/]+\\.[a-zA-Z]+$" regexp/extended))
         (sansdirectory (match:substring 
                         (regexp-exec file-name-non-directory-regexp input-file-name)))
         (file-name-non-extension-regexp (make-regexp "^[^.]+")))
    (match:substring (regexp-exec file-name-non-extension-regexp sansdirectory))
    ))

(define (nuke-shell-specials filename)
  (define (maybe-transform ch)
    (if (memq ch '(#\space #\tab #\| #\& #\; #\< #\> #\( #\) #\' #\$))
        #\_
      ch))
  (let ((result (make-string (string-length filename))))
    (let loop ((chars-processed 0))
      (if (= chars-processed (string-length filename))
          result
        (let ((this-char (string-ref filename chars-processed)))
          (string-set! result chars-processed (maybe-transform this-char))
          (loop (+ 1 chars-processed)))))))

(provide 'filenames)