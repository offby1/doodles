#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket $0
|#

#lang racket

(require
 (planet dvanhorn/flickr:2:3)
 (only-in (planet "memoize.ss" ("dherman" "memoize.plt" 2 (= 3))) define/memo*)
 (lib "etc.ss")
 (lib "match.ss")
 (lib "pretty.ss")
 (lib "trace.ss")
 "keys.ss")

(define *cache* #f)
(define (alist->mutable-hash a)
  (let ((h (make-hash '())))
    (for-each (lambda (p)
                (hash-set!
                 h
                 (car p)
                 (cdr p)))
              a)
    h))

(define-struct photo (id title) #:transparent)

;; returns a pair: the total number of pages (or 0 to indicate you
;; asked for a page past the end), and the list of photos from that
;; page.

;; This is defined at top level, rather than inside for-each-page,
;; because for some reason memoization doesn't seem to work when it's
;; defined inside for-each-page.
(define/memo* (get-one-page page-number . args)
  (when (zero? page-number)
    (error 'get-page "ah ah ah -- flickr page numbers count from 1, not 0"))

  (parameterize ((signed? #t))
    (match
     (hash-ref
      *cache*
      page-number
      (lambda ()
        (let ((got (apply flickr.photos.search
                          (list* #:page (number->string page-number)
                                 #:sort "date_posted_asc"
                                 args
                                 #:user_id (*user-id*)
                                 #:auth_token (get-preference (*pref-name*))))))
          (hash-set! *cache* page-number got)
          got)))
     [(('photos atts photos ...))
      (match atts
             [(('page this-page) ('pages pages) ('perpage _) ('total _))
              (cons (string->number pages)
                    (map (lambda (p)
                           (match p
                                  [('photo
                                    (('farm _)
                                     ('id id)
                                     ('isfamily _)
                                     ('isfriend _)
                                     ('ispublic _)
                                     ('owner _)
                                     ('secret _)
                                     ('server _)
                                     ('title title)))
                                   (make-photo id title)])) photos))])])))

;; for each page, calls proc on the list of photos from that page.
(define (for-each-page proc . args)
  (define *cache-file* (format "downloaded-photos-cache-~a.ss" (*user-id*)))
  (set! *cache*
        (if (file-exists? *cache-file*)
            (alist->mutable-hash
             (with-input-from-file *cache-file* read))
             (make-hash '())))
  (let loop ((pages-requested 0))
    (match-let ([(total-pages buncha-photos ...)
                 (apply get-one-page (add1 pages-requested) args)])
      (when (positive? total-pages)
        (proc
         buncha-photos
         (add1 pages-requested)
         total-pages)
        (loop (add1 pages-requested)))))

  (when (positive? (hash-count *cache*))
    (with-handlers
        ([exn:fail:filesystem? void])
      (call-with-output-file
          *cache-file*
        (lambda (op)
          (parameterize ((print-hash-table #t))
            (pretty-print (hash-map *cache* cons) op)))))))

(provide (all-defined-out))

(module+ main
  (require (only-in "misc.rkt" title->number-or-false))
  (define *photos-by-title* (make-hash '()))

  (for-each-page
   (lambda ( photos this-page-number total-pages)
     (when (equal? 1 this-page-number)
       (printf "Downloading from flickr ...~%"))

     (for-each
      (lambda (photo)
        (hash-set! *photos-by-title* (photo-title photo) photo)
        (printf "Noted ~s => ~s -- number is ~s~%"
                (photo-title photo)
                photo
                (title->number-or-false (photo-title photo))))
      photos)
     (printf
      (format "Downloaded ~a photos from flickr...~%"
              (hash-count *photos-by-title*)))))
  (pretty-display *photos-by-title*)
  )
