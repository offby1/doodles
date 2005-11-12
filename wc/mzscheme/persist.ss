#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

;;; Now that I've written all this, I notice that mzscheme may have
;;; the ability to save and restore images.  I used that technique
;;; with scheme48, and it was _way_ easier than this was.

;;; Thanks to `foof' (aka `ashinn') for guidance and inspiration

(module persist mzscheme
  (require (lib "serialize.ss"))
  (provide define-persistent)

;;; (define-persistent x "x.ss" body)

;;; should either read x.ss and assign the value to x, or else run the
;;; body code, save the value into x.ss, and assign it to x.

;;; I suspect there are subtle problems when the value that you save
;;; contains cycles.
  
  (define (ep . args)
    (apply fprintf (cons (current-error-port)
                         args)))
  (define-syntax define-persistent
    (syntax-rules ()
      ((_ var file body ...)
       (define var
         (with-handlers
             ((exn:fail:filesystem?
               (lambda (exn)
                 (ep "Cache ~s not found; doing expensive computation ... " file)
                 (let ((res body ...))
                   (ep "done.  Now writing file ~s ... " file)
                   (with-handlers
                       ((exn:fail:filesystem?
                         (lambda (exn) (ep "Warning: can't write file ~s~n" file))))
                     (with-output-to-file file
                       (lambda ()
                         ;; TODO -- perhaps also parameterize
                         ;; print-unreadble and other things?
                         (parameterize ((print-hash-table #t))
                           (write (serialize res)))
                         ))
                     (ep "done~n")) ; log if can't save to file
                   res))))
           (begin0
             (with-input-from-file file
               (lambda ()
                 (ep "Reading from ~s ... " file)
                 (deserialize (read))))
             (ep "done~n"))))))))
