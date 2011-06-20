#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require
 (only-in "aws-common.rkt"
          AWSAccessKeyId
          run-tests/maybe-exit
          sign
          stringy?
)
 (only-in "group.rkt" group)

 (only-in (planet neil/htmlprag:1:6) html->shtml)
 (only-in (planet offby1/offby1/zdate) zdate)
 (only-in net/url
          call/input-url
          post-impure-port
          purify-port
          string->url
          url->string
          url-host
          url-path
          url?
          )
 (only-in srfi/13 string-join)
 (only-in unstable/net/url url-path->string)
 racket/trace
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

  (define (zero-pad str num)
    (let loop ([str str])
      (if (< (string-length str)
             num)
          (loop (string-append "0" str))
          str)))

  (define (hexencode-codepoint-number i)
    (string->bytes/utf-8 (format "%~a" (string-upcase (zero-pad (number->string i 16) 2)))))

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

(define/contract (urllib-quote b #:safe safe)
  (->* (bytes? #:safe (set/c byte?))  () bytes?)

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
  (check-equal? (urllib-quote #"/~connolly/" #:safe (set 47)) #"/%7Econnolly/"))

(provide encode-alist)
;; The car of each element must be something that can be stringified
;; via (format "~a").  The cdr must be something that can be given to
;; "escape", which means either a bytes? or a string?
(define/contract (encode-alist a)
  ((listof (cons/c stringy? stringy?)) . -> . bytes?)
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

(define (add-AWS-signature-and-stuff url form-data)

  (when (string? url)
    (set! url (string->url url)))

  (let* ([boilerplate `(("AWSAccessKeyId"   . ,AWSAccessKeyId)
                        ("SignatureMethod"  . "HmacSHA256")
                        ("SignatureVersion" . "2")
                        ("Timestamp"        . ,(zdate #:offset 0))
                        ("Version"          . "2009-04-15")
                        )]
         [merged  (sort-alist (append boilerplate form-data))]
         [bytes-to-sign  (string->bytes/utf-8 (format "~a~%~a~%~a~%~a"
                                                       "POST"
                                                       (url-host url)
                                                       (url-path->string (url-path url))
                                                       (encode-alist merged)))]
         [whole-enchilada-list (cons `("Signature" . ,(sign bytes-to-sign)) merged)])

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


;; Some debugging code that helped me properly escape characters while
;; generating the signature.

;; Characters that I am probably escaping correctly.  Every time I do
;; a POST, and the server doesn't bitch at me, I add all the
;; characters in that post to this set.
(define *ok-chars* (set))
(define (bytes->charset b) (apply set (string->list (bytes->string/utf-8 b))))
(define (update-ok-chars! alist)
  (define (update-from-bytes! b)
    (set! *ok-chars* (set-union *ok-chars* (bytes->charset b))))
  (for ([p alist])
    (update-from-bytes! (car p))
    (update-from-bytes! (cdr p))))

;; Display characters that I'm probably not properly escaping.
(define (note-suspicious-characters alist)
  (fprintf (current-error-port)
           "Suspicious chars: ~a"
           (set-subtract
            (for/fold ([all-chars-in-input (set)])
                ([p alist])
                (set-union all-chars-in-input (bytes->charset (car p))
                           (set-union all-chars-in-input (bytes->charset (cdr p)))))
            *ok-chars*)))


(define/contract (post-with-signature url form-data)
  (url? (listof (cons/c bytes? bytes?)) . -> . any/c)
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
             ((200) (update-ok-chars! form-data))
             ((403)
              (note-suspicious-characters form-data)
              (fprintf (current-error-port) "Signature trouble with~%~a~%" POST-body))
             (else
              (fprintf (current-error-port) "simpledb doesn't like~%~a~%" POST-body)
              (copy-port response-inp (current-error-port))
              (newline (current-error-port))
              (error 'post-with-signature  "Bad response: ~s" headers))))
         response-inp))

     ;; actually the response is XML, but this works fine
     html->shtml

     `(,(format "host: ~a" (url-host url))
       "Content-Type: application/x-www-form-urlencoded; charset=utf-8"))))

(provide simpledb-post)
(define/contract (simpledb-post form-data)
  ((listof (cons/c bytes? bytes?)) . -> . any/c)
  (post-with-signature
   (string->url "http://sdb.amazonaws.com/")
   form-data))

(define (list-domains)
  (simpledb-post
   `((#"Action"             . #"ListDomains")
     (#"MaxNumberOfDomains" . #"100"))))

;; http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/
(define *simpledb-select-reserved-keywords*
  '("and" "asc" "between" "by" "desc" "every" "from" "in"
    "intersection" "is" "like" "limit" "not" "null"
    "or" "order" "select" "where"))

(provide escape-attribute-name)
;; it would probably be fine to just double all existing backticks,
;; and then pessimistically wrap the result in backticks, thus
;; transforming "fred" => "`fred`".  But oh well.
(define/contract (escape-attribute-name n)
  (stringy? . -> . stringy?)
  (define (bt-wrap s)
    (string-append "`" s "`"))

  (if (member n *simpledb-select-reserved-keywords*)
      (bt-wrap n)
      (let-values ([(internally-escaped safe? _)
                    (for/fold ([result ""]
                               [safe? #t]
                               [chars-seen 0])
                        ([ch n])
                        (cond
                         ;; safe characters
                         ((or (char<=? #\a (char-downcase ch) #\z)
                              (char=? ch #\_)
                              (char=? ch #\$)
                              (and (positive? chars-seen)
                                   (char<=? #\0 ch #\9)))
                          (values (string-append result (string ch))
                                  safe?
                                  (add1 chars-seen)))

                         ;; backticks get doubled
                         ((char=? ch #\`)
                          (values (string-append result (make-string 2 ch))
                                  #f
                                  (add1 chars-seen)))

                         ;; everything else gets passed through as-is,
                         ;; but the result needs to be wrapped in
                         ;; backticks.
                         (else
                          (values (string-append result (string ch))
                                  #f
                                  (add1 chars-seen)))))])
        (if safe? internally-escaped (bt-wrap internally-escaped)))))

(define-test-suite escape-tests
  (check-equal? (escape-attribute-name "or") "`or`")
  (check-equal? (escape-attribute-name "a0_$") "a0_$")
  (check-equal? (escape-attribute-name "0a_$") "`0a_$`")
  (check-equal? (escape-attribute-name "x-y") "`x-y`")
  (check-equal? (escape-attribute-name "x`y") "`x``y`"))

;; The return value is Unix time -- seconds since "the epoch"
(provide find-time-of-most-recent-entry)
(define/contract (find-time-of-most-recent-entry #:domain [domain "freenode"])
  (->* () (#:domain string?) real?)
  (let ([resp
         (simpledb-post
          `((#"Action"             . #"Select")
            (#"SelectExpression" . ,(string->bytes/utf-8
                                     (format
                                      (string-join
                                       `("select itemname()"
                                         "from ~a"
                                         "where itemname() > '0'"
                                         "order by itemname() desc"
                                         "limit 1")
                                       " ") (escape domain))))))])
    (match resp
      [(list '*TOP* _ ...
             (list 'selectresponse _
                   (list 'selectresult (list 'item (list 'name name))
                         _ ...)
                   _ ...))
       (string->number name)]
      [_ (error 'find-time-of-most-recent-entry "Unrecgonized response: ~a" resp)])))



(define-test-suite all-tests
  urllib-quote-tests
  sign-tests
  escape-tests)

(define (main . args)
  (run-tests/maybe-exit all-tests)
  (displayln
   (list-domains))
  (displayln
   (simpledb-post
    `((#"DomainName"          . #"frotz")
      (#"Action"              . #"PutAttributes")
      (#"ItemName"            . #"test")
      (#"Attribute.0.Name"    . #"action")
      (#"Attribute.0.Replace" . #"true")
      (#"Attribute.0.Value"   . #"a value with spaces")
      (#"Attribute.1.Name"    . #"snorgulous")
      (#"Attribute.1.Replace" . #"true")
      (#"Attribute.1.Value"   . ,(string->bytes/utf-8 "an ellipsis:\u2026"))
      (#"Attribute.2.Name"    . #"frotz")
      (#"Attribute.2.Replace" . #"true")
      (#"Attribute.2.Value"   . ,(string->bytes/utf-8 "a nasty Unicode character:\ufffd"))))))

(provide main)
