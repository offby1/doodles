#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require
 "contracts.rkt"
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

(define alist? (listof (cons/c stringy? stringy?)))
(define item?  (cons/c stringy? alist?))
(define batch? (listof item?))

(define/contract (append-number thing n)
  (stringy? natural-number/c  . -> . stringy?)
  (let ([transformer (cond
                      ((string? thing) values)
                      ((bytes?  thing) ensure-bytes))])
    (transformer (format "~a.~a" thing n))))

;; A kludge, to work around the lack of resultion in timestamps.
;; Appends an autoincrementing number to each timestamp.
(define/contract (uniqify-keys batch)
  (batch? . -> . batch?)
  (reverse
   (call-with-values
       (lambda ()
         (for/fold ([result '()]
                    [key-to-count (make-immutable-hash '())])
             ([item batch])

             (let* ([this-key (first item)]
                    [seen (hash-ref key-to-count this-key (const 0))])
               (values
                (cons (cons (append-number this-key seen)
                            (rest item))
                      result)
                (hash-update key-to-count this-key add1 (const 0))))))
     (lambda (batch _)
       batch))))

(define-test-suite uniqify-tests
  (check-equal? (uniqify-keys '()) '())
  (check-equal?
   (uniqify-keys
    '((#"item name")))
   '((#"item name.0")))
  (check-equal?
   (uniqify-keys
    '((#"item 1")
      (#"item 2")))
   '((#"item 1.0")
     (#"item 2.0")))
  (check-equal?
   (uniqify-keys
    '(
      ("item 1" ("k" . "v"))
      ("item 2" ("k" . "v"))
      ))
   '(
     ("item 1.0" ("k" . "v"))
     ("item 2.0" ("k" . "v"))
     )
   )
  (check-equal?
   (uniqify-keys
    '(
      ("item" ("k" . "v"))
      ("item" ("x" . "y"))
      ))
   '(
     ("item.0" ("k" . "v"))
     ("item.1" ("x" . "y"))
     )
   )
  )

(provide make-simple-db-upload-queue)
(define/contract (make-simple-db-upload-queue simpledb-post domainname)
  ((alist? . -> . any/c) string? . -> . thread-queue/c)
  (let* ([ch (make-async-channel)]
         [th (thread
              (lambda ()
                (for ([batch (group (channel->seq ch) 25)])
                  (fprintf (current-error-port)
                           "Putting ~a to domain ~a..." batch domainname)
                  (batch-put-items simpledb-post domainname (uniqify-keys batch))
                  (fprintf (current-error-port)
                           "done~%"))
                (displayln "Background upload thread exiting."
                           (current-error-port))))])
    (cons th ch)))

(provide simpledb-enqueue)
(define/contract (simpledb-enqueue queue item)
  (-> thread-queue/c any/c void)
  (async-channel-put (cdr queue) item))

(define ensure-bytes
  (match-lambda
   [(? bytes? b)
    b]
   [(? string? s)
    (string->bytes/utf-8 s)]
   [(? symbol? s)
    (ensure-bytes (symbol->string s))]
   [(? pair? p)
    (cons (ensure-bytes (car p))
          (ensure-bytes (cdr p)))]))

(define (batch-put-items simpledb-post domainname items)
  (define (batch-put-items-args domainname items)
    (for/list ([batch (group items 25)])
      `((#"DomainName"                 . ,(ensure-bytes domainname))
        (#"Action"                     . #"BatchPutAttributes")

        ,@(for/fold ([result '()])
              ([(item i) (in-indexed batch)])
              (define (prefix s) (ensure-bytes (format "Item.~a.~a" i s)))
            `(,@result
              (,(prefix "ItemName") . ,(ensure-bytes (first item)))
              ,@(map ensure-bytes (attrs->prefixed-numbered-alist prefix (rest item))))))))

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
  uniqify-tests
  number-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
