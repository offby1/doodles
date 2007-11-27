#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0"
|#
(module svn-log mzscheme
(require (lib "match.ss")
         (lib "pretty.ss")
         (only (lib "1.ss" "srfi")
               filter)
         (lib "xml.ss" "xml"))
(define *repo-URL*
  "http://svn.collab.net/repos/svn"
  ;;"file:///C:/Documents and Settings/Lenovo User/Desktop/silly-repository"
  )
(define *svn-exe* "c:/program files/subversion/bin/svn.exe")
(define *interesting-user-names* (list "joe"))

(define *first-revision* 0)
(define *total-revisions* 100)

(define-values (proc stdout-ip stdin-op stderr-ip)
  (subprocess #f #f #f *svn-exe*
              "log"
              "--xml"
              (format "-r~a:~a"
                      (number->string *first-revision*)
                      (number->string (sub1 (+ *first-revision* *total-revisions*))))
              *repo-URL*))

(define *xml-parse-errors* #f)

(define (filter-users user-names xml)
  (match xml
    [('log
      atts
      entries ...)
     (filter
      (lambda (one-entry)
        (match one-entry
          [('logentry
            (('revision r))
            ('author author-atts auth)
            ('date date-atts date)
            ('msg msg-atts msg ...))
           (member auth user-names)]))

      entries)]))

(define snarf-stdout-thread
  (thread
   (lambda ()
     (with-handlers
         ([exn:xml?
           (lambda (e)
             (set! *xml-parse-errors* (exn-message e)))])
       (let ((datum
              (parameterize ((collapse-whitespace #t))
                ((eliminate-whitespace '(log logentry) (lambda (x) x))
                 (document-element (read-xml stdout-ip))))))
         (pretty-print
          (filter-users
           *interesting-user-names*
           (xml->xexpr  datum)))))
     (close-input-port stdout-ip))))

(define *error-spew* '())
(define snarf-stderr-thread
  (thread
   (lambda ()
     (let loop ((error-lines '()))
       (let ((one-thing (read-line stderr-ip 'any)))
         (if (eof-object? one-thing)
             (set! *error-spew* (reverse error-lines))
             (loop (cons one-thing error-lines)))))
     (close-input-port stderr-ip))))


(close-output-port stdin-op)
(for-each sync (list snarf-stderr-thread snarf-stdout-thread))
(subprocess-wait proc)

(if (not (zero? (subprocess-status proc)))
    (fprintf (current-error-port)
             "Subprocess returned a failure code: ~a~%"
             *error-spew*)

    (if *xml-parse-errors*
        (fprintf (current-error-port)
                 "Subprocess didn't emit clean XML: ~a~%"
                 *xml-parse-errors*)))


(provide (all-defined))
)
