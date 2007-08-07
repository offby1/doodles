#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e '(test/text-ui pipe-tests)'
|#

;; An experiemnt.  I'm thinking that when we connect to a channel,
;; we'll generate one of these weird output ports that prefixes all
;; lines with "PRIVMSG #channel-name :", and hand it out to any client
;; who wants to write to the channel -- that way the client can just
;; (display "hey you" op), and the Right Stuff will get sent to the
;; server.

(module port-transformer mzscheme
(require (lib "trace.ss")
         (only (planet "port.ss" ("schematics" "port.plt"))
               port->string)
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2)))

;; a little like the built-in readline, but returns two values: the
;; first is the string, shorn of its line terminator; the second is
;; the actual terminator, or the eof-object.  Also doesn't take a
;; mode-symbol; instead it just looks for #\newline
(define (my-read-line ip)
  (let loop ((chars '()))
    (let ((ch (read-char ip)))
      (cond
       ((eof-object? ch)
        (if (null? chars)
            (values ch ch)
          (values (list->string (reverse chars)) ch)))
       ((char=? ch #\newline)
        (values (list->string (reverse chars)) ch))
       (else
        (loop (cons ch chars)))
       ))))

(trace my-read-line)

;; given an output port, and a proc that transforms strings, returns
;; another output port.  Everything you write to the returned output
;; port will get transformed by the proc, and that transformed string
;; will get written to the original output port.
(define (make-weird-thing original-op string-transformer-proc)
  (let-values (((ip op) (make-pipe)))
    (thread
     (lambda ()
       (let loop ()
         (let-values (((line terminator) (my-read-line ip)))
           (if (eof-object? line)
               (close-output-port original-op)
             (begin
               (display (string-transformer-proc line) original-op)
               (unless (eof-object? terminator)
                 (display terminator original-op))
               (flush-output original-op)
               (loop)))))))
    op))

(define (do-weird-thing input)
  (let-values (((ip op) (make-pipe)))
    (let ((op (make-weird-thing
               op
               (lambda (str)
                 (format "PRIVMSG #emacs :~a" str)))))
      (display input op)
      (close-output-port op)
      (port->string ip))))

(define pipe-tests
  (let ((i "Hey you"))

    (test-suite
     "pipe"
     (test-equal? "reading from weird thing"
                  (do-weird-thing i)
                  (string-append "PRIVMSG #emacs :" i))
     (test-equal? "weird thing; input has newline"
                  (do-weird-thing (string-append i "\n"))
                  (string-append "PRIVMSG #emacs :" i "\n")))))

(provide (all-defined))
)