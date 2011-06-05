#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require
 (only-in "group.rkt" group)
  (only-in "channel.rkt" channel->seq)
 racket/async-channel
 racket/trace
 rackunit
 rackunit/text-ui)

;; these let our caller give us one item at a time, and yet benefit
;; from the efficiency of a batch upload.
(define thread-queue/c  (cons/c thread? async-channel?))

(provide close-upload-queue)
(define/contract (close-upload-queue q)
  (-> thread-queue/c void)
  (async-channel-put (cdr q) eof)
  (sync (car q))
  )

(define stringy? (or/c string? bytes?))
(define alist? (listof (cons/c stringy? stringy?)))
(define item?  (cons/c stringy? alist?))
(define batch? (listof item?))

;; A kludge, to work around the lack of resultion in timestamps.
;; Perhaps I should append an autoincrementing number to each
;; timestamp, rather than nixing all but the first.  Oh well.
(define/contract (nix-duplicates batch)
  (batch? . -> . batch?)
  (for/fold ([result '()])
      ([item batch])
      (cons item result)))

(trace nix-duplicates)

(provide make-simple-db-upload-queue)
(define/contract (make-simple-db-upload-queue simpledb-post domainname)
  ((alist? . -> . any/c) string? . -> . thread-queue/c)
  (let* ([ch (make-async-channel)]
         [th (thread
              (lambda ()
                (for ([batch (group (channel->seq ch) 25)])
                  (fprintf (current-error-port)
                           "Putting ~a to domain ~a..." batch domainname)
                  (batch-put-items simpledb-post domainname (nix-duplicates batch))
                  (fprintf (current-error-port)
                           "done~%"))
                (displayln "Background upload thread exiting."
                           (current-error-port))))])
    (cons th ch)))

(provide simpledb-enqueue)
(define/contract (simpledb-enqueue queue item)
  (-> thread-queue/c any/c void)
  (async-channel-put (cdr queue) item))

(define (batch-put-items simpledb-post domainname items)
  (define (batch-put-items-args domainname items)
    (for/list ([batch (group items 25)])
      `(("DomainName"                 . ,domainname)
        ("Action"                     . "BatchPutAttributes")

        ,@(for/fold ([result '()])
              ([(item i) (in-indexed batch)])
              (define (prefix s) (format "Item.~a.~a" i s))
            `(,@result
              (,(prefix "ItemName") . ,(first item))
              ,@(attrs->prefixed-numbered-alist prefix (rest item)))))))

  (apply simpledb-post (batch-put-items-args domainname items)))

(define (attrs->numbered-alist as)
  (reverse
   (for/fold ([result '()])
       ([(kvp index) (in-indexed as)])
       (define (prefix s) (format "Attribute.~a.~a" index s))
     `((,(prefix "Value")   . ,(cdr kvp))
       (,(prefix "Replace") . "true")
       (,(prefix "Name")    . ,(car kvp))
       ,@result))))

(define (attrs->prefixed-numbered-alist prefix as)
  (map (lambda (p)
         (cons (prefix (car p))
               (cdr p)))
       (attrs->numbered-alist as)))

(define-test-suite number-tests
  (check-equal?
   (attrs->numbered-alist '(("foo" . "bar")
                            ("baz" . "ugh")))
   '(("Attribute.0.Name"    . "foo")
     ("Attribute.0.Replace" . "true")
     ("Attribute.0.Value"   . "bar")

     ("Attribute.1.Name"    . "baz")
     ("Attribute.1.Replace" . "true")
     ("Attribute.1.Value"   . "ugh"))
   ))

(define-test-suite all-tests
  number-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
