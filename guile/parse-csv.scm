;; A comma-separated-values file is zero or more LINEs.

;; Each LINE is zero or more FIELDs, separated by commas and optional
;; whitespace.

;; Each FIELD is either a Scheme string (i.e., it begins with a
;; double-quote character), or a sequence of characters not including
;; a comma.

(define (file-to-lines filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((line (read-line))
                 (results '()))
        (if (eof-object? line)
            (reverse results)
          (loop (read-line)
                (cons line results)))))))

(define (string->fields str)

  (define (read-field inp)
    (define read-string read)
    (define (read-non-string inp)
      ;; read characters up to (but not including) the first comma or
      ;; end of file.
      (let loop ((char (peek-char inp))
                 (result '()))
        (if (or (eof-object? char)
                (char=? char #\,))
            (list->string (reverse result))
          (begin
            (read-char inp)
            (loop (peek-char inp)
                  (cons char result))))))

    (let ((next-char (peek-char inp)))
      (if (eof-object? next-char)
          next-char
        (let ((result (if (char=? #\" next-char)
                          (read-string inp)
                        (read-non-string inp))))
          
          ;; consume comma if there is one.
          (set! next-char (peek-char inp))
          (if (and
               (not (eof-object? next-char))
               (char=? #\, next-char))
              (read-char inp))
          result))))

  (call-with-input-string 
   str
   (lambda (inp)
     (let loop ((field (read-field inp))
                (results '()))
       (if (eof-object? field)
           (reverse results)
         (loop (read-field inp)
               (cons field results)))))))

;; Example usage
;; (map string->fields (file-to-lines "/katie/home/katie/addresses.csv"))
