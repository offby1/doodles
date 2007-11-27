#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mred --no-init-file --mute-banner --version --require "$0"
|#
(module svn-log mzscheme
(require (lib "class.ss")
         (lib "match.ss")
         (lib "mred.ss" "mred")
         (lib "pretty.ss")
         (only (lib "1.ss" "srfi")
               filter)
         (lib "xml.ss" "xml")
         "web/flickr/mcvicker/progress-bar.ss")
(define *repo-URL*
  ;;"svn+ssh://offby1.ath.cx/home/erich/svn-repos"
  "http://svn.collab.net/repos/svn"
  ;;"file:///C:/Documents and Settings/Lenovo User/Desktop/silly-repository"
  )
(define *interesting-user-names* (list "joe"))

(define *svn-exe* "c:/program files/subversion/bin/svn.exe")

(define *first-revision* 0)
(define *total-revisions* 100)

(define (usual-exception-handler e)
  (message-box
   "Uh oh"
   (cond
    ((exn? e)
     (format "~a:~a"
             (exn-message e)
             (let ((op (open-output-string)))
               (pretty-display
                (continuation-mark-set->context (exn-continuation-marks e))
                op)
               (get-output-string op))))
    (else
     (format "Unknown exception ~s" e)))))

(define *frame*  (new frame% (label "Subversion Log Viewer")))
(define *returned-log-data* #f)
(define *do-it!-button*
  (new button% (parent *frame*) (label "Snarf the log data")
       (callback (lambda (item event)
                   (send *pb* start!)))))

(define *pb*
  (new pb%
       (label "Progress!")
       (worker-proc
        (lambda (pb)
          (with-handlers
              ([void usual-exception-handler])
            (define *xml-parse-errors* #f)
            (define-values (proc stdout-ip stdin-op stderr-ip)
              (subprocess #f #f #f *svn-exe*
                          "log"
                          "--xml"
                          (format "-r~a:~a"
                                  (number->string *first-revision*)
                                  (number->string (sub1 (+ *first-revision* *total-revisions*))))
                          *repo-URL*))

            (define snarf-stdout-thread
              (thread
               (lambda ()
                 (with-handlers
                     ([exn:xml?
                       (lambda (e)
                         (set! *xml-parse-errors* (exn-message e)))])
                   (set! *returned-log-data*
                         (parameterize ((collapse-whitespace #t))
                           ((eliminate-whitespace '(log logentry) (lambda (x) x))
                            (document-element (read-xml stdout-ip))))))
                 (close-input-port stdout-ip))))
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
                (message-box "Fail!"
                             (format
                              "Subprocess returned a failure code: ~a"
                              *error-spew*))

                (if *xml-parse-errors*
                    (message-box "Subprocess didn't emit clean XML"
                                 *xml-parse-errors*))))
          (message-box "Yo"
                       (format "~a"
                               (filter-users
                                *interesting-user-names*
                                (xml->xexpr *returned-log-data*))))
          (send pb show #f)))
       (work-to-do *total-revisions*))
  )

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

(define *error-spew* '())

(send *frame* show #t)

(provide (all-defined))
)
