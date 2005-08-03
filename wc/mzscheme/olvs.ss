#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module
    olvs                                ;one-letter-variants
    mzscheme
  (require (only (lib "1.ss" "srfi") iota))
  (provide olvs)

  (define *the-alphabet* 
    (list->vector
     (map integer->char
          (iota (- (char->integer #\z)
                   (char->integer #\a)
                   -1)
                (char->integer #\a)
                ))))
  
  (define (25-varieties word n)

    (let loop ((letters-to-examine (vector-length *the-alphabet*))
               (result '()))
      (if (zero? letters-to-examine  )
          result
        (let ((this-letter (vector-ref *the-alphabet* (sub1 letters-to-examine))))
          (if (char=? this-letter (string-ref word n))
              (loop (- letters-to-examine 1)
                    result)
            (let ((new (string-copy word)))
              (string-set! new n this-letter)
              (loop (- letters-to-examine 1)
                    (cons new
                          result))))))
      ))
  
  (define (olvs word)
    (let loop ((letters-to-examine (string-length word))
               (result '()))
      (if (zero? letters-to-examine)
          result
        (loop (sub1 letters-to-examine)
              (append (25-varieties word (sub1 letters-to-examine))
                      result))))))
