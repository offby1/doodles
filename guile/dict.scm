(require 'sort)
(require 'note)

(define add-entry #f)
(define dump #f)

(let ((dict (make-hash-table 800001)))
  (note #t)
  (set! add-entry 
        (lambda (word)
          (note 'add "added word " word #\newline) (force-output)
          (hash-set! dict word (+ 1 (or (hash-ref dict word) 0)))))

  (set! dump
        (lambda ()
          (sort (apply append (vector->list dict))
                (lambda (p1 p2)
                  (< (cdr p1)
                     (cdr p2)))))))

(lambda ()
  (with-input-from-file "the-bible"
    (lambda ()
      (let loop ((thing (read)))
        (if (eof-object? thing)
            'done
          (begin
            (add-entry thing)
            (loop (read))))))))


(require 'generic-read)

(define (read-word port)
  (generic-reader port
		  read-char
		  char-whitespace?
		  string
		  ""
		  string-append
		  ))

(define (snarf-words fn)
  (call-with-input-file fn
    (lambda (port)
      (let loop ((result '())
                 (thing (read-word port)))
        (if (eof-object? thing)
            (reverse result)
          (loop (cons thing result)
                (read-word port)))))))