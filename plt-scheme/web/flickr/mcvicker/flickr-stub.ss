#| Hey Emacs, this is -*-scheme-*- code!
|#
(module flickr-stub mzscheme
(require (lib "kw.ss")
         (lib "trace.ss")
         (lib "url.ss" "net"))

(define *flickr-fail* (make-parameter #f))

(define exn:flickr? void)
(define (exn:flickr-code) 'it-didnt-work)
(define (exn:flickr-message . args)
  "It didn't work!  What can I say?")

(define sign-all? (make-parameter #f))

(define-syntax (define-stub stx)
  (syntax-case stx ()
    ((_ (proc-name . arglist) body)
     (syntax
      (begin
        (define proc-name
          (lambda arglist
            (if (*flickr-fail*)
                (error 'proc-name "Things just fail a lot around here")
                body)))
        (trace proc-name))))))

(define-stub (flickr.auth.getFrob . args)
  '((frob () "I'm a frob.  What you lookin' at?")))

(define-stub (flickr.auth.getToken . args)
  '((auth ()
          (token () "Yeah, I'm a token")
          (perms () "I'm some permissions")
          (user ((fullname "Bob Zemeckis")
                 (nsid "SOME-NSID-OR-OTHER")
                 (username "bobz"))))))

(define-stub (authorize-url . args)
  (string->url "file:///"))

(define-stub (flickr.photos.setMeta . args)
  '(whatever you say boss))

(define-stub (flickr.photos.setDates . args)
  '(whatever you say boss))

(define-stub (flickr.photos.search . args)
  (if (equal? (keyword-get args #:page) "3")
      '((photos
         ((page "3")
          (pages "0")
          (perpage "1000")
          (total "987654321"))))

      '((photos
         ((page "1")
          (pages "2")
          (perpage "1000")
          (total "987654321"))
         (photo
          ((farm _)
           (id "Some photo's ID")
           (isfamily _)
           (isfriend _)
           (ispublic _)
           (owner _)
           (secret _)
           (server _)
           (title "Some photo's title")))))))

(define non-text-tags (make-parameter '()))
(define current-api-key (make-parameter #f))
(define current-sec-key (make-parameter #f))
(provide (all-defined))
)
