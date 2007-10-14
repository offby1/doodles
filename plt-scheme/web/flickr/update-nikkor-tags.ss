#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#
(module update-nikkor-tags mzscheme
(require (only (lib "1.ss" "srfi") iota)
         (only (lib "pretty.ss")
               pretty-display
               pretty-print)
         (lib "kw.ss")
         (lib "trace.ss")
         (only (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
               sxpath))


;; for each of my flickr photos
;;   if it was taken with my D200
;;     find the focal length and aperture
;;     find a tag that describes that lens
;;     add that tag to the picture

(define *my-NSID*
  "20825469@N00"
;;; (car ((sxpath '(user @ nsid *text*))
;;;                         (flickr.people.findByUsername
;;;                          'username "offby1")))
  )

(define *the-channel* (make-channel))

(define snarfer
  (thread
   (lambda ()
     (fprintf (current-error-port)
              "Snarfing from flickr~%")
     (let ((total-pages #f))
       (let loop ((pages-requested 0))
         (if (or (not total-pages)
                 (< pages-requested total-pages))
             (let ((one-page (search-stub #:page (add1 pages-requested))))
               (set! total-pages (string->number (car ((sxpath '(photos @ pages *text*)) one-page))))
               (for-each (lambda (p)
                           (channel-put *the-channel* p))
                         ((sxpath '(photos (photo))) one-page))
               (loop (add1 pages-requested)))
             (channel-put *the-channel* #f)
             ))))))

;; a quick stub that acts vaguely like flickr.photos.search
(define/kw (search-stub #:key
                        [page 1]
                        [per_page 3])
  (let* (
         ;; completely arbitrary
         (total 10)
         (num-on-last-page (remainder total per_page))
         (pages (+ (quotient total per_page)
                   (if (zero? num-on-last-page)
                       0
                       1))))

    ;; Yeah, this is ugly.  Do better.
    (when (zero? num-on-last-page)
      (set! num-on-last-page per_page))

    (cons '*TOP*
          (cons 'photos
                (cons
                 `(@ (total ,(number->string total))
                     (perpage ,(number->string per_page))
                     (pages ,(number->string pages))
                     (page ,(number->string page)))
                 (map
                  (lambda (n)
                    `(photo
                      (@ (title "Yours Truly")
                         (server "2305")
                         (secret "c8c4e9bf53")
                         (owner "20825469@N00")
                         (ispublic "1")
                         (isfriend "0")
                         (isfamily "0")
                         (id ,(number->string n))
                         (farm "3"))))
                  (iota per_page (* (sub1 page) per_page))))))))

(let ((all-my-photos
       (
        search-stub
        #:page      1
        )))
  (printf "Golly, here's all my photos:~%")
  (let loop ()
    (let ((one-photo (channel-get *the-channel*)))
      (when one-photo
        (pretty-display one-photo)
        (loop))))
  )



)
