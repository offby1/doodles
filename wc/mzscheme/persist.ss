#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

;;; Thanks to `foof' (aka `ashinn') for guidance and inspiration

(module persist mzscheme
  (require (lib "serialize.ss"))
  (provide define-persistent)

;;; (define-persistent x "x.ss" body)

;;; should either read x.ss and assign the value to x, or else run the
;;; body code, save the value into x.ss, and assign it to x.

;;; I suspect there are subtle problems when the value that you save
;;; contains cycles.
  
  (define-syntax define-persistent
    (syntax-rules ()
      ((_ var file body ...)
       (define var
         (with-handlers
             ((exn:fail:filesystem?
               (lambda (exn)
                 (fprintf (current-error-port) "Cache ~s not found; doing expensive computation ... " file)
                 (let ((res body ...))
                   (with-handlers
                       ((exn:fail:filesystem?
                         (lambda (exn) (fprintf (current-error-port) "Warning: can't write file ~s~n" file))))
                     (with-output-to-file file
                       (lambda ()
                         ;; TODO -- perhaps also parameterize
                         ;; print-unreadble and other things?
                         (parameterize ((print-hash-table #t))
                           (write (serialize res)))
                         ))
                     (fprintf (current-error-port) "wrote ~s~n" file)) ; log if can't save to file
                   res))))
           (begin0
             (with-input-from-file file
               (lambda ()
                 (fprintf (current-error-port) "Reading from ~s ... " file)
                 (deserialize (read))))
             (fprintf (current-error-port) "done~n"))))))))
