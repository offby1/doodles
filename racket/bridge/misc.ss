#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module misc mzscheme
  (provide
   index
   *compass-directions*
   first-n)
  (define (index item seq )
    (- (length seq) (length (member item seq))))
  (define *compass-directions*
     `(north east south west))

  (define (first-n l n)
    (let loop ([l l][n n])
      (if (or (null? l)
              (zero? n))
          null
        (cons (car l) (loop (cdr l) (sub1 n)))))))
