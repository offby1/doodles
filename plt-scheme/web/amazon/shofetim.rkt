#lang racket

;;;;; Dependacies
(require srfi/13)
(require net/url)
(require net/base64)
(require racket/date)
(require file/md5)
(require web-server/stuffers/hmac-sha1)

;;;; Settings
(define secret-key "")
(define access-id "")
(define s3-host "s3.amazonaws.com/")
;;(define site "")
(define base-url
  (string-append "http://" s3-host))

;;;; Functions
(define (make-header key value)
  (string-append key value))

(define (date-header date)
  (make-header "Date: " date))

(define (content-type mime)
  (make-header "Content-Type: " mime))

(define (content-length len)
  (make-header "Content-Length: " (number->string len)))

(define (content-md5 md5)
  (make-header "Content-MD5: " md5))

(define (rfc2822-date)
  (date-display-format 'rfc2822)
  (date->string (seconds->date (current-seconds)) #t))

(define (weave lst sep)
  (if (null?  (cdr lst))
      (car lst)
      (string-append (car lst) sep (weave (cdr lst) sep))))

(define (aws-s3-auth-str verb md5 mime expiration amz-headers resource)
  (let ((sep "\n"))
    (if (null? amz-headers)
        (weave (list verb md5 mime expiration resource) sep)
        (weave (list verb md5 mime expiration (weave amz-headers sep) resource) sep))))

(define (s3-get resource)
  (get-impure-port
   (string->url
    (string-append base-url resource))))

(define (s3-put filename)
  (when (file-exists? filename)
    (let* ((size (file-size filename))
           (infile (open-input-file filename))
           (buffer (read-bytes size infile))
           (close-input-port infile))
      (put-object buffer filename))))

(define (aws-s3-auth-mac key str)
  (string-trim-both
   (bytes->string/utf-8 (base64-encode (HMAC-SHA1 (string->bytes/utf-8 key)
                                                  (string->bytes/utf-8 str))))))

(define (authorization-header key secret auth-str)
  (string-append "Authorization: AWS " key
                 ":" (aws-s3-auth-mac secret auth-str)))


(define (put-object buffer filename)
  (let* ((size (bytes-length buffer))
         (hash64 (bytes->string/utf-8 (let ((enc (base64-encode (md5 buffer #f))))
                                        (subbytes enc 0 (- (bytes-length enc) 2))))) ;; base64-encode adds a bogus \r\n
         (mime "binary/octet-stream")
         (datetime (rfc2822-date))
         (url (string->url (string-append base-url filename)))
         (http-headers (list (date-header datetime)
                             (content-type mime)
                             (content-md5 hash64)
                             (authorization-header access-id secret-key
                                                   (aws-s3-auth-str "PUT" hash64 mime datetime '()
                                                                    (string-append "/"))))))
    (put-impure-port url buffer http-headers)))

;;;Put it to work
(copy-port (s3-put "/etc/passwd") (current-output-port)) ;;Specify the file name in the same directory to be uploaded.
