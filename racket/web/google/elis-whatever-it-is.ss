#lang scheme

(require xml net/url-structs net/uri-codec)

(require net/url-sig net/url-unit
         net/tcp-sig net/tcp-unit
         net/ssl-tcp-unit)

;; This will give us ssl:--- for an ssl version of url.ss, then abstract in a
;; convenient function.
(define-values/invoke-unit
  (compound-unit/infer (import) (export url^) (link tcp@ url@))
  (import) (export url^))
(define ssl-tcp@ (make-ssl-tcp@ #f #f #f #f #f #f #f))
(define-values/invoke-unit
  (compound-unit (import) (export URL)
                 (link [((TCP : tcp^)) ssl-tcp@]
                       [((URL : url^)) url@ TCP]))
  (import) (export (prefix ssl: url^)))

(define (POST->port url alist)
  (let* ([url (if (string? url) (string->url url) url)]
         [scheme (url-scheme url)])
    ((if (equal? "http" scheme) post-pure-port ssl:post-pure-port)
     url
     (string->bytes/utf-8 (alist->form-urlencoded alist))
     '(#"Content-type: application/x-www-form-urlencoded"))))

(define (GET->port url headers)
  (let* ([url (if (string? url) (string->url url) url)]
         [scheme (url-scheme url)])
    ((if (equal? "http" scheme) get-pure-port ssl:get-pure-port) url headers)))

;; ----------------------------------------------------------------------------
;; Google stuff

(define google-auth
  (make-parameter
   #f (lambda (x)
        (bytes-append #"Authorization: GoogleLogin auth="
                      (if (string? x) (string->bytes/utf-8 x) x)))))

(define (url->port url)
  (GET->port url `(,(google-auth) #"GData-Version: 2")))

(define (url->xexpr url)
  (xml->xexpr (document-element (read-xml (url->port url)))))

(define (authenticate user passwd [service "xapi"])
  (define (parse-lines lines)
    (map (lambda (line)
           (let ([m (regexp-match #rx"^([^=]+)=(.*)$" line)])
             (if m
                 (cons (string->symbol (cadr m)) (caddr m))
                 (error 'login "unexpected reply line: ~s" line))))
         lines))
  (define reply
    ((compose parse-lines port->lines POST->port)
     "https://www.google.com/accounts/ClientLogin"
     `([accountType . "HOSTED_OR_GOOGLE"]
       [Email . ,user]
       [Passwd .  ,passwd]
       [service . ,service]
       [source . "PLTScheme-MzScheme-4"])))
  (cond [(assq 'Auth reply) => cdr]
        [(assq 'Error reply)
         => (lambda (reason)
              (error 'authenticate "bad username/password: ~a" (cdr reason)))]
        [else (error 'authenticate
                     "unexpected reply (no Auth or Error): ~s" reply)]))

(define (get-spreadsheets)
  (sort
   (filter-map
    (match-lambda
     [(list-no-order (list 'title _ title)
                     (list 'content (list-no-order (list 'src url) _ ...))
                     _ ...)
      (cons title url)]
     [_ #f])
    (url->xexpr (string-append "https://spreadsheets.google.com/feeds/"
                               "spreadsheets/private/full")))
   string<?
   #:key car))

(define (sheet-url->list-feed-url url)
  (match (url->xexpr url)
    [(list-no-order
      (cons 'entry (list-no-order (list 'content
                                        (list-no-order (list 'src x) _ ...))
                                  _ ...))
      _ ...)
     x]
    [_ #f]))

(define (sheet-url->cell-feed-url url)
  (match (url->xexpr url)
    [(list-no-order
      (cons 'entry (list-no-order
                    (list 'link
                          (list-no-order
                           (list 'href x)
                           (list 'rel (regexp #rx"^http://.*#cellsfeed$"))
                           _ ...))
                    _ ...))
      _ ...)
     x]
    [_ #f]))

(define (get-cells url)
  (define max-row 0)
  (define max-col 0)
  ;; get a flat list of (<row> <col> <data>)
  (define cells
    (reverse
     (filter-map
      (match-lambda
       [(cons 'entry
              (list-no-order
               (list 'gs:cell
                     (list-no-order (list 'row row) (list 'col col) _ ...)
                     x)
               _ ...))
        (let ([row (string->number row)] [col (string->number col)])
          (when (> row max-row) (set! max-row row))
          (when (> col max-col) (set! max-col col))
          (list row col x))]
       [_ #f])
      (url->xexpr url))))
  (let loop ([row max-row] [col max-col] [line '()] [table '()] [cells cells])
    (cond [(zero? row) table]
          [(zero? col) (loop (sub1 row) max-col '() (cons line table) cells)]
          [else (let ([x (and (pair? cells)
                              (= row (caar cells))
                              (= col (cadar cells))
                              (caddar cells))])
                  (loop row (sub1 col) (cons x line) table
                        (if x (cdr cells) cells)))])))

;; ----------------------------------------------------------------------------
;; Table formatting, for fun

(provide print-table filler)

(define-struct filler (str) #:omit-define-syntaxes)

(define (filler x)
  (let ([str (if (string? x) x (format "~a" x))])
    (if (equal? str "")
        (error 'filler "empty filler: ~e" x)
        (make-filler str))))

(define (format-filler str width)
  (let* ([n (ceiling (/ width (string-length str)))]
         [str (if (= n 1) str
                  (string-append* (for/list ([i (in-range n)]) str)))])
    (if (= width (string-length str)) str (substring str 0 width))))

(define (format-string str width alignment)
  (let ([len (string-length str)])
    (if (>= len width) str
        (let ([pad (make-string (- width len) #\space)])
          (if (eq? alignment 'l)
              (string-append str pad)
              (string-append pad str))))))

(define (print-table table alignments [convert #f])
  (define table*
    (for/list ([row table])
      (for/list ([x row])
        (if (filler? x) x
            (let ([x (if convert (convert x) x)])
              (if (string? x) x (format "~a" x)))))))
  (define (col-width . col)
    (foldl (lambda (x acc)
             (if (filler? x) acc (max acc (string-length x))))
           0 col))
  (define widths (apply map col-width table*))
  (for ([row table*])
    (let* ([line (map (lambda (x width alignment)
                        (if (filler? x)
                            (format-filler (filler-str x) width)
                            (format-string x width alignment)))
                      row widths alignments)]
           [line (string-append* (add-between line " "))]
           [line (regexp-replace #rx" +$" line "")])
      (display line)
      (newline))))

;; ----------------------------------------------------------------------------
;; "User Interface"

(define (ask prompt)
  (printf "~a: " prompt) (flush-output)
  (read-line))

(define (choose prompt options)
  (printf "~a:\n" prompt)
  (for ([i (in-naturals 1)] [o options]) (printf "  ~a. ~a\n" i o))
  (sub1 (read)))

;; ----------------------------------------------------------------------------

(parameterize ([google-auth
                (authenticate (or (getenv "GOOGLE_USER")
                                  (ask "Google username (with domain)"))
                              (or (getenv "GOOGLE_PASSWORD")
                                  (ask "Password"))
                              "wise")])
  (define sheets (get-spreadsheets))
  (define sheet-url
    (cdr (list-ref sheets (choose "Choose a spreadsheet" (map car sheets)))))
  (define cells-url (sheet-url->cell-feed-url sheet-url))
  (define table (get-cells cells-url))
  (define (row-of x) (map (lambda (_) x) (car table)))
  (define hline (row-of (filler "-")))
  (print-table `(,hline ,@table ,hline) (row-of 'l) (lambda (x) (or x ""))))
