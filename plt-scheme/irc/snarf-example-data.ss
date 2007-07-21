#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module snarf-example-data mzscheme
(require (lib "pretty.ss")
         "parse-message.ss")
(print-hash-table #t)
(define prefixes (make-hash-table 'equal))
(define commands (make-hash-table 'equal))
(define param-strings (make-hash-table 'equal))
(define (note! table item)
  (hash-table-put! table item (add1 (hash-table-get table item 0))))

(call-with-input-file "example input"
  (lambda (ip)
    (let loop ()
      (let ((str (read ip)))
        (when (not (eof-object? str))
          (let-values (((prefix command params)
                        (parse-message str)))
            (note! prefixes prefix)
            (note! commands command)
            (note! param-strings params)
            (loop)))))))

(for-each
 pretty-print
 (list prefixes commands param-strings))
)