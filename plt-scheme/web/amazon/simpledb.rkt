#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require
 (only-in "aws-common.ss" AWSAccessKeyId sign)
 (only-in (planet neil/htmlprag:1:6) html->shtml)
 (only-in (planet offby1/offby1/zdate) zdate)
 (only-in net/uri-codec alist->form-urlencoded form-urlencoded->alist)
 (only-in net/url url-host url-path call/input-url post-impure-port purify-port string->url url->string)
 (only-in srfi/13 string-join)
 (only-in unstable/net/url url-path->string)
 rackunit
 rackunit/text-ui
 )

;; Attempt to follow the rules at
;; http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/REST_RESTAuth.html

(define (sort-alist query-string-components)
  (define (thing->bytes/utf-8 t)
    (cond
     ((bytes? t) t)                     ;assume it's UTF-8 already --
                                        ;probably difficult to ensure

     ((string? t) (string->bytes/utf-8 t))
     ((symbol? t) (thing->bytes/utf-8 (symbol->string t)))
     ((number? t) (thing->bytes/utf-8 (number->string t)))))
  (sort query-string-components bytes<? #:key (compose thing->bytes/utf-8 car)))

;; http://docs.python.org/library/urllib.html#module-urllib
#|
urllib.quote(string[, safe])
Replace special characters in string using the %xx escape. Letters, digits, and the characters '_.-' are never quoted. By default, this function is intended for quoting the path section of the URL.The optional safe parameter specifies additional characters that should not be quoted — its default value is '/'.

Example: quote('/~connolly/') yields '/%7econnolly/'.
|#

(define/contract (byte->urllib-quoted-bytes b #:safe safe)
  (-> byte? #:safe (set/c byte?) bytes?)

  (define (hexencode-codepoint-number i)
    (string->bytes/utf-8 (format "%~a" (string-upcase (number->string i 16)))))

  (cond
   ((<= 65 b 90)                        ;upper-case letter
    (bytes b))
   ((<= 97 b 122)                       ;lower-case letter
    (bytes b))
   ((<= 48 b 57)                        ;digit
    (bytes b))
   ((set-member? safe b)
    (bytes b))
   (else
    (hexencode-codepoint-number b))))

(define/contract (urllib-quote b #:safe [safe (set 47)])
  (->* (bytes?)  (#:safe (set/c byte?)) bytes?)

  (for/fold ([result #""])
      ([c (in-bytes b)])
      (bytes-append result (byte->urllib-quoted-bytes c #:safe safe))))

(define/contract escape
  (-> (or/c string? bytes?) bytes?)
  (match-lambda
   [(? bytes? b)
    (urllib-quote b #:safe (apply set (bytes->list #".-_~")))]
   [(? string? s)
    (escape (string->bytes/utf-8 s))]))

(define-test-suite urllib-quote-tests
  (check-equal? (urllib-quote #"/~connolly/") #"/%7Econnolly/"))

(define (encode-alist a)
  (bytes-join
   (reverse
    (for/fold ([result '()])
        ([p (in-list a)])
        (cons
         (string->bytes/utf-8
          (format "~a=~a"
                  (car p)
                  (escape (cdr p))))
         result)))
   #"&"))

;; For debugging -- so I can run this code, and then the python code
;; (which similarly goes in five-minute chunks) and have a prayer of
;; getting the same timestamp (and hence, the same HMAC signature) on
;; both.
(define (now-rounded [resolution-seconds 300])
  (let-values ([(q r)
                (quotient/remainder (current-seconds) resolution-seconds)])
    (* resolution-seconds q)))

(define (add-AWS-signature-and-stuff url form-data)

  (when (string? url)
    (set! url (string->url url)))

  (let* ([boilerplate `(("AWSAccessKeyId"   . ,AWSAccessKeyId)
                        ("SignatureMethod"  . "HmacSHA256")
                        ("SignatureVersion" . "2")
                        ("Timestamp"        . ,(zdate (now-rounded) #:offset 0)
                                              )
                        ("Version"          . "2009-04-15")
                        )]
         [merged  (sort-alist (append boilerplate form-data))]
         [string-to-sign  (string->bytes/utf-8 (format "~a~%~a~%~a~%~a"
                                                       "POST"
                                                       (url-host url)
                                                       (url-path->string (url-path url))
                                                       (encode-alist merged)))]
         [whole-enchilada-list (cons `("Signature" . ,(bytes->string/utf-8 (sign string-to-sign))) merged)])

    whole-enchilada-list))

(define-test-suite sign-tests
  (match-let ([(list (cons "Signature"        sig)
                     (cons "AWSAccessKeyId"   key)
                     (cons "Hugger"           hugger)
                     (cons "SignatureMethod"  meth)
                     (cons "SignatureVersion" sigver)
                     (cons "Timestamp"        time)
                     (cons "Version"          ver))
               (add-AWS-signature-and-stuff
                "http://frotz"
                '(("Hugger" . "mugger")))])

    (check-equal? key    AWSAccessKeyId)
    (check-equal? meth   "HmacSHA256")
    (check-equal? sigver "2")
    (check-equal? hugger "mugger")))

(define (post-with-signature url form-data)
  (let ([POST-body (encode-alist (add-AWS-signature-and-stuff url form-data))])
    (call/input-url
     url
     (lambda (url headers)
       (let* ([response-inp
               (post-impure-port
                url
                POST-body
                headers)]
              [headers (purify-port response-inp)])
         (match-let ([(pregexp "^HTTP/(.*?) (.*?) (.*?)\r.*" (list _ vers code message)) headers])
           (case (string->number code)
             ((200) 'good-good)
             (else
              (fprintf (current-error-port) "simpledb doesn't like~%~a~%" POST-body)
              (copy-port response-inp (current-error-port))
              (newline (current-error-port))
              (error 'post-with-signature  "Bad response: ~s" headers))))
         response-inp))

     ;; actually the response is XML, but this works fine
     html->shtml

     `(,(format "host: ~a" (url-host url))
       "Content-Type: application/x-www-form-urlencoded; charset=utf-8"))
    POST-body))

(provide simpledb-post)
(define (simpledb-post form-data)
  (post-with-signature
   (string->url "http://sdb.amazonaws.com/")
   form-data))

(define (list-domains)
  (simpledb-post
   `(("Action" . "ListDomains")
     ("MaxNumberOfDomains" . "100"))))

(define-test-suite all-tests
  urllib-quote-tests
  sign-tests)

(define (main . args)
  (let ([failures (run-tests all-tests)])
    (when (positive? failures)
      (exit 1)))
  (printf "Domains: ~a~%" (list-domains))
  (write
   (simpledb-post
    `(("DomainName"          . "frotz")
      ("Action"              . "PutAttributes")
      ("ItemName"            . "test")
      ("Attribute.0.Name"    . "action")
      ("Attribute.0.Replace" . "true")
      ("Attribute.0.Value"   . "a value with spaces")

      ("Attribute.1.Name"    . "snorgulous")
      ("Attribute.1.Replace" . "true")
      ("Attribute.1.Value"   . "an ellipsis:\u2026")

      ("Attribute.2.Name"    . "frotz")
      ("Attribute.2.Replace" . "true")
      ("Attribute.2.Value"   . "a nasty Unicode character:\ufffd"))))
  (newline))

(provide main)
